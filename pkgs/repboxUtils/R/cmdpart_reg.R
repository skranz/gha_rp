# Create cmdpart representation of Stata regression commands

example = function() {
  cmdlines = c(
    "ivregress 2sls y1 x1 (y2 y3 = z1 z2), vce(robust)",
    "capt: xi: regress y i.i1##c.d1 [aw=x] if a == 5 in 3, vce(hc2)",
    #"capture quietly xi: regress y i.i1##c.d1, vce(hc2)",
    "capt: regress y i.i1##c.d1 if a==5 [aw=z], vce (robust)  opt2(arg2 = fun( funarg )) noarg"
  )

  cmdlines = c(
    'reg logpgp95 sjb1500 if logpgp95!=. & catho80!=.& muslim80!=. & notmcp80!=. & coal!=. & goldm!=. & iron!=. & silv!=. & zinc!=. & oilres!=. & humid1!=. & temp1!=. & steplow!=. & deslow!=. & stepmid!=. & desmid!=. &  drystep!=. & hiland!=. & drywint!=. & acdummy==1 & wbname!="United States" & wbname!="Canada" & wbname!="Australia" & wbname!="New Zealand" & wbname!="Singapore" & wbname!="Hong Kong, China"  & aa_urb!=., rob',
    'reg x y',
    'reg x y if country = "china, japan, germany"'
  )


  options(warn=2)
  df = cmdparts_of_stata_reg(cmdlines)
}



cmdparts_of_stata_reg = function(cmdlines) {
  restore.point("stata_reg_cmdpart")

  if (length(cmdlines)==0) return(data.frame(str_row = integer(0), parent=character(0), part=character(0), content=character(0), tag=character(0), counter=integer(0), runid=integer(0)))

  str = trimws(cmdlines)
  # Replace tabs with spaces
  # Otherwise we wont correctly store the cmd
  # variable
  str = gsub("\t"," ", str, fixed=TRUE)


  # For some really tricky if conditions or similar
  # We first replace brackets and quotes by placeholders
  # reg y x1 if (i==1) | i==2 | inlist(f1, "A" "B") [aw=x1] in 5/25, robust   level( 95  )

  txt = paste0(str, collapse = "\n")


  # Deal with , in quotes e.g. in if conditions
  qpho = try(blocks.to.placeholder(txt, start='"', end='"', ph.prefix = "#~qu"))
  if (!is(qpho, "try-error"))
    txt = qpho$str


  pho = try(blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br"))
  if (is(pho,"try-error")) {
    pho = stepwise.blocks.to.placeholder(str)
  }




  # Normalize to ensure space before if/in when directly following a closing bracket, quote, or placeholder
  pho$str = stringi::stri_replace_all_regex(pho$str, "(?<=[\\)\\]\"']|~#)if\\s", " if ")
  pho$str = stringi::stri_replace_all_regex(pho$str, "(?<=[\\)\\]\"']|~#)in\\s", " in ")

  # In our example we have now
  # str = " if #~br1~# | i==2 | inlist#~br2~# [aw=x1] in 5/25 , robust"
  str = strsplit(pho$str,split = "\n")[[1]]
  if (length(str)==0) str = ""
  ph.df = pho$ph.df



  cp = cp_init(str)

  while(TRUE) {
    cp = cp_jump_ws(cp)
    cp = cp_add_starts_with(cp,c("quietly:","quiet:","qui:","quietly ","quiet ","qui ", "capture ","capture:","cap ", "cap:","capt ", "capt:"),"pre","cap_quiet",use_counter=TRUE)
    if (!cp$did_change) break
  }
  cp$str
  substring(cp$str, cp$start)

  # Find colon prefixes before command
  while(TRUE) {
    cp = cp_jump_ws(cp)
    cp = cp_add_left_of(cp,":","pre","",use_counter=TRUE, include_split=TRUE)
    if (!cp$did_change) break
  }

  cp$str
  substring(cp$str, cp$start)
  # Set brackets () into ph
  # res = set_cmdpart_block(str, cp, "(", ")","()","",counter=TRUE)
  # str = res$str; cp = res$cp

  cp = cp_jump_ws(cp)
  cp = cp_add_left_of(cp,"( )|(,)|$|(\\[)", "cmd", "",fixed = FALSE)



  cp = cp_jump_ws(cp)
  cp = cp_add_left_of(cp, "( in )|( if )|(\\[)|,|$", "varlist","", use_counter=FALSE, fixed=FALSE)



  # varlist ############################################

  # Pass the placeholder dictionary to cp so the sub-functions can resolve brackets
  cp$ph_df <- ph.df
  cp <- cmdpart_process_reg_varlists(cp)
  df = cp$df
  cp$str

  # weight_str, in_str and if_str #################################

  # Now it is getting a bit more complicated
  # We want to set if_str, in_str and weight_str
  # but they may appear in different orders or not at all

  # a) weight_str
  sub_start = cp$start
  sub_end = stri_locate_first_fixed(cp$str,",")[,1]-1
  sub_end = ifelse(is.na(sub_end),nchar(cp$str), sub_end )

  sstr = substring(cp$str, sub_start, sub_end)

  weight_start = stri_locate_first_fixed(sstr, "[")[,1] + sub_start -1
  weight_end = stri_locate_first_fixed(sstr, "]")[,2] + sub_start -1


  str_rows = which(!is.na(weight_start))

  if (length(str_rows)>0) {
    weight_str = substring(cp$str[str_rows], weight_start[str_rows], weight_end[str_rows])

    cp$str[str_rows] = stringi::stri_sub_replace(cp$str[str_rows], from=weight_start[str_rows], to=weight_end[str_rows], replacement="{{weight_str}}")



    equal_pos = stringi::stri_locate_first_fixed(weight_str,"=")[,1]
    has_weight_type = !is.na(equal_pos)
    weight_type = ifelse(has_weight_type,substring(weight_str,2, equal_pos-1) %>% trimws(), NA)
    var_start = ifelse(has_weight_type,equal_pos+1,2 )
    weight_var = substring(weight_str,var_start, nchar(weight_str)-1) %>% trimws()
    weight_str = ifelse(has_weight_type,
      paste0("[{{weight_type}}={{weight_var}}]"),
      paste0("[{{weight_var}}]")
    )

    n_add = NROW(weight_str)*3
    if (cp$n + n_add >= NROW(cp$df)) {
      cp$df = cp_add_empty_df(cp$df, n_add*2)
    }

    # 1a) add weight_str
    new.n = cp$n+length(weight_str)
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "main"
    cp$df$str_row[inds] = str_rows
    cp$df$part[inds] = "weight_str"
    cp$df$content[inds] = weight_str
    cp$n = new.n

    # 1b) Add weight_var
    new.n = cp$n+length(weight_str)
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "weight_str"
    cp$df$str_row[inds] = str_rows
    cp$df$part[inds] = "weight_var"
    cp$df$content[inds] = weight_var
    cp$n = new.n

    # 1c) Add weight_type
    new.n = cp$n+length(weight_str)
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "weight_type"
    cp$df$str_row[inds] = str_rows
    cp$df$part[inds] = "weight_type"
    cp$df$content[inds] = weight_type

    cp$n = new.n

  }

  # 2. if_str
  sub_end = stri_locate_first_fixed(cp$str,",")[,1]-1
  sub_end = ifelse(is.na(sub_end),nchar(cp$str), sub_end )
  sstr = substring(cp$str, sub_start, sub_end)

  if_start = stri_locate_first_fixed(sstr, " if ")[,1] + sub_start
  sstr = substring(sstr, if_start-sub_start+1)
  if_end = stri_locate_first_regex(sstr, "( in )|(\\{\\{)")[,1] + if_start - 2
  if_end = ifelse(is.na(if_end),sub_end, if_end)

  str_rows = which(!is.na(if_start))
  if_start = if_start[str_rows]; if_end = if_end[str_rows]
  cp = cp_add(cp,str_rows,if_start,if_end,"if_str","",ignore.right.ws=TRUE)

  cp$str

  # 3. in_str

  sub_end = stri_locate_first_fixed(cp$str,",")[,1]-1
  sub_end = ifelse(is.na(sub_end),nchar(cp$str), sub_end )
  sstr = substring(cp$str, sub_start, sub_end)

  in_start = stri_locate_first_fixed(sstr, " in ")[,1] + sub_start
  sstr = substring(sstr, in_start-sub_start+1)
  in_end = stri_locate_first_fixed(sstr, "{{")[,1] + in_start - 2
  in_end = ifelse(is.na(in_end),sub_end, in_end)

  str_rows = which(!is.na(in_start))
  in_start = in_start[str_rows]; in_end = in_end[str_rows]
  cp = cp_add(cp,str_rows,in_start,in_end,"in_str","",ignore.right.ws=TRUE)

  cp$str

  # options ###########################################

  # cp$str now looks e.g. like
  # {{cmd}} {{varlist}} {{if_str}} {{weight_str}} {{in_str}}, robust   level#~br3~#

  # We now replace bracket placeholders again since option parsing deals on its own with brackets
  #restore.point("cmdpart_of_reg_opts")
  cp$str = replace.placeholders(cp$str, ph.df)
  cp$df$content = replace.ph.keep.lines(cp$df$content, ph.df)


  start = stri_locate_first_regex(cp$str,",")[,1]+1
  end = nchar(cp$str)
  str_rows = which(!is.na(start))

  make_opts = FALSE
  if (length(str_rows) > 0) {
    start = start[str_rows]; end = end[str_rows]
    all_opt_str = substring(cp$str[str_rows],start)
    res = cmdpart_parse_stata_opt_str(all_opt_str)
    lens = lapply(res$opt_str,length)
    make_opts = (any(lens>0))
  }
  if (make_opts) {
    # Replace str
    opt_ph = sapply(res$opt_str, function(x) paste0("{{opt_str", seq_along(x),"}}", collapse=" "))
    cp$str[str_rows] = stringi::stri_sub_replace(cp$str[str_rows], from=start, to=end, replacement=opt_ph)

    if (FALSE) {
      test_li = list(
        str_row = unlist(mapply(rep,x=str_rows, times=lens,SIMPLIFY = FALSE)),
        parent = "main",
        opt_str = unlist(res$opt_str),
        opt = unlist(res$opt),
        opt_arg = unlist(res$opt_arg)
      )
      sapply(test_li, length)
    }

    my_unlist = function(x, empty = character(0)) {
      res = unlist(x)
      if (is.null(res)) return(empty)
      res
    }

    opt_df = tibble(
        str_row = unlist(mapply(rep,x=str_rows, times=lens,SIMPLIFY = FALSE)),
        parent = rep("main", length(str_row)),
        opt_str = my_unlist(res$opt_str),
        opt = my_unlist(res$opt),
        opt_arg = my_unlist(res$opt_arg)
      ) %>%
      group_by(str_row) %>%
      mutate(opt_num = seq_len(n())) %>%
      ungroup()

    n_add = NROW(opt_df)*2 + sum(!is.na(opt_df$opt_arg))
    if (cp$n + n_add >= NROW(cp$df)) {
      cp$df = cp_add_empty_df(cp$df, n_add*2)
    }

    # 4a) add opt_str
    new.n = (cp$n+NROW(opt_df))
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "main"
    cp$df$str_row[inds] = opt_df$str_row
    cp$df$part[inds] = "opt_str"
    cp$df$counter[inds] = opt_df$opt_num
    cp$df$content[inds] = opt_df$opt_str

    cp$n = new.n

    # 4b) add opt
    new.n = (cp$n+NROW(opt_df))
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = paste0("opt_str")
    cp$df$str_row[inds] = opt_df$str_row
    cp$df$part[inds] = "opt"
    cp$df$counter[inds] = opt_df$opt_num
    cp$df$content[inds] = opt_df$opt

    cp$n = new.n

    # 4c) add opt_arg where it exists
    rows = which(!is.na(opt_df$opt_arg))

    if (length(rows)>0) {
      new.n = cp$n+length(rows)
      inds = (cp$n+1):new.n
      cp$df$parent[inds] = "opt_str"
      cp$df$str_row[inds] = opt_df$str_row[rows]
      cp$df$part[inds] = "opt_arg"
      cp$df$counter[inds] = opt_df$opt_num[rows]
      cp$df$content[inds] = opt_df$opt_arg[rows]

      cp$n = new.n
    }

  }

  # adapt tags and df ##########################################

  df = cp$df[seq_len(cp$n),]
  rows = which(df$tag == "cap_quiet")
  if (length(rows)>0) {
    is_capture = has.substr(df$content[rows], "cap")
    df$tag[rows[is_capture]] = "capture"
    df$tag[rows[!is_capture]] = "quietly"
  }

  cont = trimws(df$content)
  rows = which(df$part == "pre")
  if (length(rows) > 0) {
    df$content[rows] = gsub("[ \t]+\\:",":", df$content[rows])
    irows = rows[trimws(df$content[rows]) == "xi:"]
    df$tag[irows] = "xi"
  }

  rows = which(df$part == "cmd" | df$part == "subcmd")
  df$tag[rows] = df$content[rows]

  df$content[seq_along(cp$str)] = cp$str


  opt_rows = which(df$part == "opt" & df$content %in% c("vce","robust","cluster","r","ro","rob","robu","robus","cl","clu","clus","clust","cluste","cluster",""))
  df$tag[opt_rows] = "se"

  opt_str_rows = cmdpart_find_parent_rows(df, opt_rows)
  df$tag[opt_str_rows] = "se"

  opt_arg_rows = cmdpart_find_child_rows(df, opt_str_rows, "opt_arg") %>% na.omit()
  df$tag[opt_arg_rows] = "se"


  opt_rows = which(df$part == "opt" & df$content %in% c("absorb"))
  df$tag[opt_rows] = "absorb"

  opt_str_rows = cmdpart_find_parent_rows(df, opt_rows)
  df$tag[opt_str_rows] = "absorb"

  opt_arg_rows = cmdpart_find_child_rows(df, opt_str_rows, "opt_arg") %>% na.omit()
  df$tag[opt_arg_rows] = "absorb"


  # substitute quoted strings back
  restore.point("replace_quotes")
  if (!is(qpho, "try-error"))
    df$content = replace.ph.keep.lines(df$content,qpho$ph.df)

  df
}



#' Process all varlists in a cmdpart object
#'
#' This replaces the old loop in `cmdparts_of_stata_reg` that relied on the
#' legacy `parse.stata.reg.vars` function.
#' Extracts depvar, endo, exo, instr
#' Does NOT require dataset information (No column matching or glob expansion).
#'
#' @param cp The cmdpart object being built
#' @return The updated cmdpart object
cmdpart_process_reg_varlists = function(cp) {
  cmds = cp$df$content[cp$df$part == "cmd"]
  vl_rows = which(cp$df$part == "varlist")
  varlists = cp$df$content[vl_rows]

  for (i in seq_along(cmds)) {
    str_row = cp$df$str_row[vl_rows[i]]
    cmd = cmds[i]
    varlist_str = varlists[i]

    # Replace bracket placeholders if they were swapped out earlier
    if (!is.null(cp$ph_df)) {
      varlist_str = replace.placeholders(varlist_str, cp$ph_df)
    }

    # 1. Syntactic parsing of the varlist string into tokens
    tokens_df = cmdpart_parse_varlist(cmd, varlist_str)
    if (is.null(tokens_df) || nrow(tokens_df) == 0) next

    # 2. Extract and handle subcmd (e.g., '2sls' in 'ivregress 2sls')
    subcmd_row = tokens_df[tokens_df$part == "subcmd", ]
    if (nrow(subcmd_row) > 0) {
      # Adjust the original string to include the subcmd placeholder
      cp$str[str_row] = gsub("{{varlist}}", "{{subcmd}} {{varlist}}", cp$str[str_row], fixed = TRUE)
      cp$start[str_row] = cp$start[str_row] + nchar("{{subcmd}} ")

      # Register the subcmd in the cmdpart df
      cp = cp_add_part_in_df(cp, str_rows = str_row, part = "subcmd", content = subcmd_row$content)
    }

    # 3. Register the variable tokens
    v_rows = tokens_df[tokens_df$part == "v", ]
    if (nrow(v_rows) > 0) {
      cp = cp_add_part_in_df(
        cp,
        str_rows = str_row,
        part = "v",
        content = v_rows$content,
        tag = v_rows$tag,
        counter = seq_len(nrow(v_rows)),
        parent = "varlist"
      )

      # 4. Reconstruct the {{varlist}} content string with placeholders
      dep_ph = "{{v1}}"

      exo_idx = which(v_rows$tag == "exo")
      exo_ph = if (length(exo_idx) > 0) paste0("{{v", exo_idx, "}}", collapse = " ") else ""

      endo_idx = which(v_rows$tag == "endo")
      endo_ph = if (length(endo_idx) > 0) paste0("{{v", endo_idx, "}}", collapse = " ") else ""

      instr_idx = which(v_rows$tag == "instr")
      instr_ph = if (length(instr_idx) > 0) paste0("{{v", instr_idx, "}}", collapse = " ") else ""

      # Build the new string preserving IV parenthesis structure
      if (length(endo_idx) == 0 && length(instr_idx) == 0) {
        new_content = paste(c(dep_ph, if (nchar(exo_ph) > 0) exo_ph else NULL), collapse = " ")
      } else {
        new_content = paste0(
          dep_ph,
          if (nchar(exo_ph) > 0) paste0(" ", exo_ph) else "",
          " (", endo_ph, " = ", instr_ph, ")"
        )
      }

      cp$df$content[vl_rows[i]] = trimws(new_content)
    }
  }

  return(cp)
}

#' Parse a Stata varlist syntactically based on the command type
cmdpart_parse_varlist = function(cmd, varlist_str) {
  varlist_str = trimws(varlist_str)

  # Normalize spaces around dashes to keep ranges (e.g., "var1 - var5") as a single token
  varlist_str = gsub("\\s*-\\s*", "-", varlist_str)

  iv_commands = c("ivregress", "ivreg2", "ivreg", "xtivreg2", "xtivreg", "ivreghdfe", "reg2hdfespatial")
  is_iv_cmd = cmd %in% iv_commands
  is_reghdfe = cmd == "reghdfe"

  # Check if there is an explicit Instrumental Variable block: (endo = instr)
  has_iv_syntax = grepl("\\(.*=.*\\)", varlist_str)

  if ((is_iv_cmd || is_reghdfe) && has_iv_syntax) {
    return(cmdpart_parse_iv_varlist(cmd, varlist_str))
  } else {
    return(cmdpart_parse_default_varlist(varlist_str))
  }
}

#' Parse standard OLS varlists
cmdpart_parse_default_varlist = function(varlist_str) {
  tokens = extract_varlist_tokens(varlist_str)

  if (length(tokens) == 0) return(NULL)

  depvar = tokens[1]
  exo = if (length(tokens) > 1) tokens[2:length(tokens)] else character(0)

  df = tibble::tibble(
    part = "v",
    tag = c("depvar", rep("exo", length(exo))),
    content = c(depvar, exo),
    parent = "varlist"
  )
  return(df)
}
#' Split a string by spaces, but keep content within parentheses/brackets together.
extract_varlist_tokens = function(str) {
  if (is.na(str) || trimws(str) == "") return(character(0))

  chars = strsplit(str, "")[[1]]
  tokens = character()

  # preallocate
  token_chars = character(length(chars))
  token_len = 0

  level = 0
  for (ch in chars) {
    if (ch == "(") level = level + 1
    else if (ch == ")") {
      if (level > 0) level = level - 1
    }

    if (level == 0 && (ch == " " || ch == "\t" || ch == "\n" || ch == "\r")) {
      if (token_len > 0) {
        tokens = c(tokens, paste(token_chars[1:token_len], collapse = ""))
        token_len = 0
      }
    } else {
      token_len = token_len + 1
      token_chars[token_len] = ch
    }
  }
  if (token_len > 0) {
    tokens = c(tokens, paste(token_chars[1:token_len], collapse = ""))
  }

  tokens
}

#' Parse IV varlists including subcommands and parenthesis blocks
cmdpart_parse_iv_varlist = function(cmd, varlist_str) {
  subcmd = NULL

  # Extract specific subcmd for `ivregress` (e.g., "2sls", "gmm")
  if (cmd == "ivregress") {
    parts = strsplit(varlist_str, "\\s+", perl = TRUE)[[1]]
    subcmd = parts[1]
    # Strip subcmd from the beginning of the varlist
    varlist_str = sub(paste0("^", subcmd, "\\s+"), "", varlist_str)
    varlist_str = trimws(varlist_str)
  }

  # Locate the (endo = instr) block safely
  brace_pos = locate_1st_level_braces(varlist_str, open="(", close=")")

  iv_block_idx = NA
  if (!is.null(brace_pos) && nrow(brace_pos) > 0) {
    for (i in seq_len(nrow(brace_pos))) {
      block_str = substr(varlist_str, brace_pos[i, 1] + 1, brace_pos[i, 2] - 1)

      # Mask inner braces to find top-level '='
      inner_braces = locate_1st_level_braces(block_str, open="(", close=")")
      safe_block = block_str
      if (!is.null(inner_braces) && nrow(inner_braces) > 0) {
        for (j in seq_len(nrow(inner_braces))) {
          len = inner_braces[j, 2] - inner_braces[j, 1] + 1
          substring(safe_block, inner_braces[j, 1], inner_braces[j, 2]) <- strrep(" ", len)
        }
      }

      if (grepl("=", safe_block)) {
        iv_block_idx = i
        break
      }
    }
  }

  if (!is.na(iv_block_idx)) {
    br_start = brace_pos[iv_block_idx, 1]
    br_end = brace_pos[iv_block_idx, 2]

    iv_block = substr(varlist_str, br_start + 1, br_end - 1)

    # Everything outside the parentheses are either the depvar or exogenous variables
    outside_before = trimws(substr(varlist_str, 1, br_start - 1))
    outside_after = trimws(substr(varlist_str, br_end + 1, nchar(varlist_str)))
    outside_str = trimws(paste(outside_before, outside_after))

    # Parse IV block
    # Find '=' at the top level of iv_block
    inner_braces = locate_1st_level_braces(iv_block, open="(", close=")")
    safe_block = iv_block
    if (!is.null(inner_braces) && nrow(inner_braces) > 0) {
      for (j in seq_len(nrow(inner_braces))) {
        len = inner_braces[j, 2] - inner_braces[j, 1] + 1
        substring(safe_block, inner_braces[j, 1], inner_braces[j, 2]) <- strrep(" ", len)
      }
    }
    eq_pos = regexpr("=", safe_block)

    if (eq_pos > 0) {
      endo_str = trimws(substr(iv_block, 1, eq_pos - 1))
      instr_str = trimws(substr(iv_block, eq_pos + 1, nchar(iv_block)))

      endo = extract_varlist_tokens(endo_str)
      instr = extract_varlist_tokens(instr_str)
    } else {
      # Fallback if '=' is missing inside parenthesis
      endo = character(0)
      instr = extract_varlist_tokens(iv_block)
    }

    # Parse Outside block
    outside_tokens = extract_varlist_tokens(outside_str)

    depvar = if (length(outside_tokens) > 0) outside_tokens[1] else character(0)
    exo = if (length(outside_tokens) > 1) outside_tokens[2:length(outside_tokens)] else character(0)

  } else {
    # Fallback to standard OLS parsing if IV regex logic fails
    return(cmdpart_parse_default_varlist(varlist_str))
  }

  # Construct token dataframe
  df = tibble::tibble(
    part = c(if (!is.null(subcmd)) "subcmd" else character(0),
             if (length(depvar) > 0) "v" else character(0),
             rep("v", length(exo) + length(endo) + length(instr))),
    tag = c(if (!is.null(subcmd)) "" else character(0),
            if (length(depvar) > 0) "depvar" else character(0),
            rep("exo", length(exo)),
            rep("endo", length(endo)),
            rep("instr", length(instr))),
    content = c(if (!is.null(subcmd)) subcmd else character(0),
                depvar, exo, endo, instr),
    parent = c(if (!is.null(subcmd)) "varlist" else character(0),
               rep("varlist", length(depvar) + length(exo) + length(endo) + length(instr)))
  )

  return(df)
}
