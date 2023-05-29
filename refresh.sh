aws s3 --profile rw-research-team cp s3://ethswarm-research-team/data.rds data.rds
Rscript data_refresh.R
aws s3 --profile rw-research-team cp data.rds s3://ethswarm-research-team/
