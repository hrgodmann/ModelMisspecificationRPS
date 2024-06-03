

dat <- rio::import("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/raw/mona_chiara_rps.xlsx")


# Draws
sum(sum((dat$Chiara == "r" & dat$Mona == "r")),
sum((dat$Chiara == "p" & dat$Mona == "p")),
sum((dat$Chiara == "s" & dat$Mona == "s")))


# win Mona
sum(sum((dat$Mona == "s" & dat$Chiara == "p")),
sum((dat$Mona == "p" & dat$Chiara == "r")),
sum((dat$Mona == "r" & dat$Chiara == "s")))

# win Chiara
sum(sum((dat$Chiara == "s" & dat$Mona == "p")),
sum((dat$Chiara == "p" & dat$Mona == "r")),
sum((dat$Chiara == "r" & dat$Mona == "s")))


dat$combination <- paste(dat$Mona, dat$Chiara, sep = "")
table(dat$combination)

# 1 = rock, 2 = paper, 3 = scissors
dat2 <- dat

dat2$Mona <- gsub("r", 1, dat2$Mona)
dat2$Mona <- gsub("p", 2, dat2$Mona)
dat2$Mona <- gsub("s", 3, dat2$Mona)

dat2$Chiara <- gsub("r", 1, dat2$Chiara)
dat2$Chiara <- gsub("p", 2, dat2$Chiara)
dat2$Chiara <- gsub("s", 3, dat2$Chiara)

write.csv(dat2, "/Users/henrikgodmann/Desktop/rps_teaching/real_data/real_data.csv", row.names = F)


dat_mona <- dat2[c(2,3,1)]
names(dat_mona) <- c("PlayerChoice", "PlayerPrevChoice", "OpponentPrevChoice")

dat_mona$PlayerPrevChoice <- c(0, dat_mona$PlayerChoice[1:249])
dat_mona$OpponentPrevChoice <- c(0, dat_mona$OpponentPrevChoice[1:249])

write.csv(dat_mona, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/dat_mona.csv", row.names = F)


dat_chiara <- dat2[c(1,3,2)]
names(dat_chiara) <- c("PlayerChoice", "PlayerPrevChoice", "OpponentPrevChoice")

dat_chiara$PlayerPrevChoice <- c(0, dat_chiara$PlayerChoice[1:249])
dat_chiara$OpponentPrevChoice <- c(0, dat_chiara$OpponentPrevChoice[1:249])

write.csv(dat_chiara, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/dat_chiara.csv", row.names = F)


dat_dice <- rio::import("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/raw/rps_dice.xlsx")


names(dat_dice) <- c("PlayerChoice","OpponentPrevChoice")
dat_dice$PlayerPrevChoice <- c(0, dat_dice$PlayerChoice[1:249])
dat_dice$OpponentPrevChoice <- c(0, dat_dice$OpponentPrevChoice[1:249])

dat_dice <- dat_dice[c(1,3,2)]


dat_dice$PlayerChoice <- gsub("r", 1, dat_dice$PlayerChoice)
dat_dice$PlayerChoice <- gsub("p", 2, dat_dice$PlayerChoice)
dat_dice$PlayerChoice <- gsub("s", 3, dat_dice$PlayerChoice)

dat_dice$PlayerPrevChoice <- gsub("r", 1, dat_dice$PlayerPrevChoice)
dat_dice$PlayerPrevChoice <- gsub("p", 2, dat_dice$PlayerPrevChoice)
dat_dice$PlayerPrevChoice <- gsub("s", 3, dat_dice$PlayerPrevChoice)

dat_dice$OpponentPrevChoice <- gsub("r", 1, dat_dice$OpponentPrevChoice)
dat_dice$OpponentPrevChoice <- gsub("p", 2, dat_dice$OpponentPrevChoice)
dat_dice$OpponentPrevChoice <- gsub("s", 3, dat_dice$OpponentPrevChoice)

write.csv(dat_dice, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/dat_dice.csv", row.names = F)



