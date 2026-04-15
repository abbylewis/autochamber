# Choose files

Choose files

## Usage

``` r
choose_files(
  input_folder,
  l0_file_path,
  reprocess = F,
  start_date = NULL,
  end_date = NULL,
  files_to_exclude = NULL
)
```

## Arguments

- input_folder:

  Folder where input .dat files are stored

- l0_file_path:

  L0 file path

- reprocess:

  Whether or not to re-process calcualted fluxes (T/F)

- start_date:

  Start date of files to process

- end_date:

  End date of files to process

- files_to_exclude:

  Vector of file names to exclude

## Value

List of files
