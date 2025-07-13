last_commit <- system("git log -1 --format=%cd", intern = TRUE)
saveRDS(last_commit, file = "./data/last_commit.rds")

