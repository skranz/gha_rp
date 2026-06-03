Absolutely. Using `stringi` is a great choice here since it handles Perl-compatible regular expressions natively and is often faster and more robust with UTF-8 encodings (which is something you are already actively handling in your parsing).

We will replace the base R `gsub(..., perl=TRUE)` calls with `stringi::stri_replace_all_regex()`.

Here are the revised modification blocks:

!MODIFICATION extract.if.in.using repboxStata/R/parse.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/parse.R"
function_name = "extract.if.in.using"
description = "Normalize string using stringi to ensure a space before if, in, using if they follow a closing bracket, quote, or placeholder."
---
```r
extract.if.in.using = function(str) {
  restore.point("extract.if.in.using")
  #str = c("dothings x using x if a>5","", "dothings x if a>5 using x")

  # Normalize to ensure space before if/in/using when directly following a closing bracket, quote, or placeholder
  str = stringi::stri_replace_all_regex(str, "(?<=[\\)\\]\"']|~#)if\\s", " if ")
  str = stringi::stri_replace_all_regex(str, "(?<=[\\)\\]\"']|~#)in\\s", " in ")
  str = stringi::stri_replace_all_regex(str, "(?<=[\\)\\]\"']|~#)using\\s", " using ")

  patterns = c(" if ", " in ", " using ")
  keys = c("if", "in", "using")

  pos = pos.end = matrix(NA, nrow=length(str), ncol=length(keys))
  #colnames(pos) = keys
  col = 1
  for (col in seq_along(keys)) {
    col.pos = str.locate.first(str, patterns[col])
    pos[,col] = col.pos[,1]
    pos.end[,col] = col.pos[,2]
  }
  pos[is.na(pos)] = -Inf
  pos.start = pos
  max.col(pos)

  col = 3
  key.str = replicate(length(keys), rep(NA_character_, length(str)),simplify = FALSE)
  names(key.str) = keys

  changed = FALSE
  while(TRUE) {
    for (col in rev(seq_along(keys))) {
      has.col = is.finite(pos[,col]) & max.col(pos) == col
      if (any(has.col)) {
        changed = TRUE
      } else {
        next
      }
      key.str[[col]][has.col] = substring(str[has.col], pos.end[has.col,col]+1) %>% trimws()
      str[has.col] = substring(str[has.col],1,pos[has.col,col]-1)
      pos = pos[,-col, drop=FALSE]
      pos.start[has.col, col] = -Inf
    }
    if (!changed) break
    changed = FALSE
    pos = pos.start
  }

  return(list(str = str,parts=key.str))

}
```
!END_MODIFICATION extract.if.in.using repboxStata/R/parse.R

!MODIFICATION cmdparts_of_stata_reg repboxUtils/R/cmdpart_reg.R
scope = "function"
file = "/home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R"
function_name = "cmdparts_of_stata_reg"
description = "Normalize string using stringi to ensure a space before if and in if they follow a closing bracket, quote, or placeholder."
---
```r
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
  restore.point("cmdpart_of_reg_opts")
  #options(warn=2)
  #disable.restore.points(!TRUE)
  #undebug(replace.placeholders)
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


  restore.point("jslkfslfhksdfh")

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


  df
}
```
!END_MODIFICATION cmdparts_of_stata_reg repboxUtils/R/cmdpart_reg.R
