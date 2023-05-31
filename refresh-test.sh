aws s3 cp s3://ethswarm-research-team/data-test2.rds data-test2.rds
Rscript -e "install.packages('tidyverse')"
Rscript data_refresh.R short
aws s3 cp data-test2.rds s3://ethswarm-research-team/
