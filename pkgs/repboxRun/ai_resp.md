This is a great idea. Replication packages frequently contain nested `.zip` files (e.g., authors zip up specific datasets or code folders before zipping the whole supplement). Relying on R's internal `unzip()` (like what is currently done in `unzip.zips()` in `file_utils.R`) often throws false "corrupted zip" errors, so moving this capability securely into the command-line powered `robust_unzip()` is the best approach.

Below is a complete proposal on how to implement this, passing the configuration all the way down from the top-level `rb_options()` to the actual system call.

### 1. Update `robust_unzip()` in `init.R`
We can add a `nested_unzip = TRUE` argument and implement a `while` loop to iteratively discover and extract newly unzipped `.zip` files. We track processed zips to prevent infinite loops (in case the zip files aren't deleted after extraction).

```r
# FILE: init.R
# R's internal unzip command too often thinks the ZIP file of a replication package
# is corrupted even though linux command line unzip is able to unzip it.
robust_unzip = function(zipfile, exdir, files=NULL, overwrite=TRUE, verbose=FALSE, nested_unzip=TRUE, remove_nested_zip=FALSE) {
  restore.point("robust_unzip")
  
  if (overwrite) {
    cmd = paste0("unzip -o ")
  } else {
    cmd = paste0("unzip -n ")
  }

  if (is.null(files)) {
    cmd = paste0(cmd, '"', normalizePath(zipfile), '" -d "', normalizePath(exdir, mustWork = FALSE), '"')
  } else {
    cmd = paste0(cmd, '"', normalizePath(zipfile), '" ', paste0('"', files, '"', collapse=" "), ' -d "', normalizePath(exdir, mustWork = FALSE), '"')
  }
  
  if (verbose) cat(cmd, "\n")
  system(cmd, ignore.stdout = !verbose)
  
  # Recursively handle nested zip files
  if (nested_unzip) {
    # Track zips to avoid infinite loops
    processed_zips = normalizePath(zipfile, mustWork=FALSE)
    
    while (TRUE) {
      # Scan exdir for newly extracted .zip files
      current_zips = list.files(exdir, pattern = "\\.zip$", ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
      current_zips = normalizePath(current_zips, mustWork = FALSE)
      
      new_zips = setdiff(current_zips, processed_zips)
      
      if (length(new_zips) == 0) {
        break # No more nested zips to extract
      }
      
      for (nz in new_zips) {
        if (verbose) cat("\nExtracting nested zip:", nz)
        # Extract the nested zip into the directory where it resides
        robust_unzip(
          zipfile = nz, 
          exdir = dirname(nz), 
          files = NULL, 
          overwrite = overwrite, 
          verbose = verbose, 
          nested_unzip = FALSE # We handle depth dynamically in the while loop
        )
        
        processed_zips = c(processed_zips, nz)
        
        if (remove_nested_zip) {
          file.remove(nz)
        }
      }
    }
  }
}
```

### 2. Update Options in `rb.R` and `run_opts.R`
Add the `nested_unzip` parameter with a default of `TRUE` to your global options configuration.

```r
# FILE: rb.R (Update rb_options)
rb_options = function(install_from = "local_ejd", stop.on.error = TRUE, stata_version = 17, store_data_caches=TRUE, timeout = 60*5, remove_existing_problems=TRUE, nested_unzip=TRUE, stata_opts = NULL, r_opts = NULL, problem_fail_action= if(stop.on.error) "error" else "msg", ...) {
  opts = as.list(environment())
  if (is.null(stata_opts)) {
    if (require(repboxStata, quietly = TRUE))
      opts$stata_opts = repbox_stata_opts(timeout = timeout,all.do.timeout = timeout)
  }
  if (is.null(r_opts)) {
    if (require(repboxR, quietly = TRUE))
      opts$r_opts = repbox_r_opts()
  }
  opts
}
```

```r
# FILE: run_opts.R (Update repbox_run_opts)
repbox_run_opts = function(stop.on.error = TRUE, stata_version = 17, slimify = FALSE, slimify_org=slimify, store_data_caches=TRUE, timeout = 60*5, remove_existing_problems=TRUE, make_script_parcel=TRUE, nested_unzip=TRUE, stata_opts = repbox_stata_opts(timeout = timeout,all.do.timeout = timeout), r_opts = repbox_r_opts(), art_opts = repbox_art_opts(), map_opts=repbox_map_opts(), html_opts = repbox_html_opts(), problem_fail_action= if(stop.on.error) "error" else "msg") {
  list(
    stop.on.error = stop.on.error,
    problem_fail_action = problem_fail_action,
    stata_version = stata_version,
    timeout = timeout,
    store_data_caches = store_data_caches,
    slimify = slimify,
    slimify_org = slimify,
    remove_existing_problems = remove_existing_problems,
    make_script_parcel = make_script_parcel,
    nested_unzip = nested_unzip,  # <-- Added
    stata_opts = stata_opts,
    r_opts = r_opts,
    art_opts = art_opts,
    map_opts = map_opts,
    html_opts = html_opts
  )
}
```

### 3. Propagate the setting down the call stack
You need to pass the setting from `rb$opts$nested_unzip` through `rb_extract_zip_to_org()` and down to `robust_unzip()`.

**Step A:** Update `rb_update_file_info_parcel()` in `rb_file_info.R`:
```r
# FILE: rb_file_info.R
rb_update_file_info_parcel = function(rb, overwrite=FALSE, unzip_org=TRUE, assume_org_complete=FALSE, parcels=rb$parcels) {
  # ... (existing code)
    if (!is.empty(zip_file)) {
      rb = rb_load_parcels(rb, "file_info")
      if (!rb_has_complete_org_dir(rb=rb)) {
        # Grab the nested_unzip flag from options
        nested_unzip = if(!is.null(rb$opts$nested_unzip)) rb$opts$nested_unzip else TRUE
        
        rb_extract_zip_to_org(project_dir, zip_file, nested_unzip = nested_unzip)
      }
    }
  # ... (existing code)
}
```

**Step B:** Update `rb_extract_zip_to_org()` in `rb_sup_zip.R`:
```r
# FILE: rb_sup_zip.R
rb_extract_zip_to_org = function(project_dir, zip_file=rb_get_sup_zip(project_dir), ..., overwrite=TRUE, nested_unzip=TRUE) {
  restore.point("rb_extract_zip_file")
  if (is.empty(zip_file)) return()
  if (!overwrite) {
    if (rb_has_complete_org_dir(project_dir = project_dir, zip_file = zip_file))
      return()
  }
  repbox_sup_extract_zip(project_dir, sup_zip = zip_file, just_extract_code = FALSE, nested_unzip = nested_unzip)
}
```

**Step C:** Update `repbox_sup_extract_zip()` in `init.R`:
```r
# FILE: init.R
repbox_sup_extract_zip = function(project_dir, sup_zip, just_extract_code, remove_macosx_dirs=TRUE, nested_unzip=TRUE) {
  restore.point("repbox_sup_just_extract_code")
  # ... (existing setup code)
  
  if (!just_extract_code) {
    robust_unzip(sup_zip, exdir = org.dir, nested_unzip = nested_unzip)
    if (file.exists(just_extract_code_file)) file.remove(just_extract_code_file)
    note_problem_if_no_sub_files(project_dir, file_df = file_df)
    return(TRUE)
  }
  
  # ... (existing code for just_extract_code)
}
```

### Discussion & Further Recommendations
1. **Handling `__MACOSX` directories:** Because MacOS often packs duplicate hidden `.DS_Store` and metadata files into nested zip files, `robust_unzip()` might extract new `__MACOSX` directories. You are already calling `remove_macosx_dirs(org.dir)` directly inside `repbox_init_project()`, which searches recursively, so these will still be successfully cleaned up.
2. **Deleting nested zip files (`remove_nested_zip`):** I've defaulted `remove_nested_zip = FALSE` in `robust_unzip`. It's usually safer not to aggressively delete the original `.zip` payloads off the disk in case of hashing/provenance checks later. You already have `slimify_org.dir()` that runs later to prune non-essential items, which is much safer.
3. **Refactoring `unzip.zips()`:** In `file_utils.R`, you have `unzip.zips(dir, remove=TRUE)` which uses R's internal `unzip()`. Now that `robust_unzip` recursively checks `.zip`s, you might consider swapping out `try(unzip(zip.file, ...))` for `try(robust_unzip(zip.file, ...))` inside `unzip.zips()` to fix the internal corrupted zip issue you documented.
