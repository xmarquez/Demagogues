
files_resources <- list(partition = "parallel", memory = "12G", ncpus = 2,
                                  walltime = "6:00:00")

dfm_resources <- list(partition = "parallel", memory = "45G", ncpus = 2,
                      walltime = "6:00:00")

splits_resources <- list(partition = "parallel", memory = "20G", ncpus = 2,
                         walltime = "0:02:00")

fcm_resources <- list(partition = "bigmem", memory = "128G", ncpus = 2,
                      walltime = "0:40:00")

predictive_model_resources <- list(partition = "parallel", memory = "50G",
                                   ncpus = 2, walltime = "5:00:00")

evaluation_model_resources <- list(partition = "parallel", memory = "15G",
                                   ncpus = 2, walltime = "0:10:00")

svd_word_vectors_resources <- list(partition = "parallel", memory = "25G", ncpus = 10,
                                   walltime = "0:10:00")

glove_word_vectors_resources <- list(partition = "parallel", memory = "1G", ncpus = 40,
                                     walltime = "1:00:00")
