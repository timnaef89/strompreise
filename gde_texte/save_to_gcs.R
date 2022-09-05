Sys.setenv(GCS_AUTH_FILE = "gdeomat_acct.json")
library(googleCloudStorageR)

all_newsmls <- list.files("test_newsml", full.names = T)
zip("all_newsmls.zip", all_newsmls)
gcs_upload("all_newsmls.zip", bucket = "gdeomat-abstimmungen-2021")

