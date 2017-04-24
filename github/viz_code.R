# Project visualization code:

# Individual contributors breakdown of contributor type by election cycle -------------
# Stacked bar chart $ and %

ind_all <- bind_rows(ind04, ind06, ind08, ind10, ind12)

ind_summary <- ind_all %>% group_by(cycle) %>% summarize
ind_summary <- select(ind_all,cycle,contCandTotal,contIETotal,contPACTotal,contPtyTotal)

ind_summary <- ind_summary %>% group_by(cycle) %>% summarize_each(funs(sum))

ind_summary <- ind_summary %>% gather("Recipient","amount",2:5)
ind_summary$amount <- ind_summary$amount/1000000000
ind_summary <- ind_summary %>% group_by(cycle) %>% mutate(total = sum(amount))
ind_summary <- ind_summary %>% group_by(cycle) %>% mutate(percent = (amount/total)*100)
colnames(ind_summary) <- c("cycle","Recipient", "$, Billions","total","Percent(%)")
ind_summary$Recipient[ind_summary$Recipient == "contCandTotal"] <- "Candidate"
ind_summary$Recipient[ind_summary$Recipient == "contIETotal"] <- "IE/SuperPAC"
ind_summary$Recipient[ind_summary$Recipient == "contPACTotal"] <- "PAC"
ind_summary$Recipient[ind_summary$Recipient == "contPtyTotal"] <- "Party"
ind_summary$cycle <- as.factor(ind_summary$cycle)
ind_summary <- select(ind_summary,-total)

ind_summary <- ind_summary %>% gather(type,total,3:4)

a <- ggplot(data = ind_summary, aes(y=amount, x= cycle, fill=Recipient))
a + geom_bar(stat = "identity") +  scale_y_continuous(name="Total Individual Contributions ($,billions)", breaks = c(1000000000,2000000000,3000000000,4000000000,5000000000),labels=c(1,2,3,4,5)) + scale_x_discrete(name="Election Cycle") + labs(color = "Recipient")

b <- ggplot(data = ind_summary, aes(y=percent, x= cycle, fill=Recipient))
b + geom_bar(stat = "identity") +  scale_y_continuous(name="Total Individual Contributions (%)") + scale_x_discrete(name="Election Cycle")

d <- ggplot(ind_summary,aes(x=cycle,y=total))+ geom_bar(stat = "identity", position = "stack", aes(fill=Recipient))
d + facet_wrap(~ type, scales="free_y") + theme_minimal() +  xlab("Election Cycle") + ylab("Total Individual Contributions") + scale_x_discrete(name="Election Cycle")


rm(ind_all, ind_summary,a,b)

# Individual contributors breakdown frequent/infrequent  by election cycle
# Stacked bar chart #, $ and %

ind_all2 <- bind_rows(select(ind04,bonica.cid, cycle),select(ind06,bonica.cid,cycle),select(ind08,bonica.cid,cycle),select(ind10,bonica.cid,cycle),select(ind12,bonica.cid,cycle))
ind_all <- count(ind_all2, bonica.cid)
ind_all2 <- left_join(ind_all2,ind_all)

ind_all2$cont <- 1

ind_all2$freq2 <- "infrequent"
ind_all2$freq2[ind_all2$n > 1] <- "frequent"

ind_all2$freq3 <- "infrequent"
ind_all2$freq3[ind_all2$n > 2] <- "frequent"

ind_all2$freq4 <- "infrequent"
ind_all2$freq4[ind_all2$n > 3] <- "frequent"

ind_all2$freq5 <- "infrequent"
ind_all2$freq5[ind_all2$n > 4] <- "frequent"

ind_all2$freq6 <- "infrequent"
ind_all2$freq6[ind_all2$n > 5] <- "frequent"


ind_all_count <- ind_all2 %>% group_by(cycle,freq3) %>% summarise(count = sum(cont))

c <- ggplot(ind_all_count,aes(x = cycle, y = count, fill=freq3))
c + geom_bar(stat="identity")

# Frequent/Infrequent SuperPAC/Non-SuperPAC by Candidate Type -----------------

ind_bar <- left_join(ind12,select(ind,id,freqContrib,ie12),by=c("bonica.cid" = "id"))

freq <- filter(ind_bar,freqContrib == 1)
infreq<- filter(ind_bar,freqContrib == 0)

ie <- filter(ind_bar,ie12 == 1)
noIE<- filter(ind_bar, ie12 == 0)

### Bar Charts
# frequent donors
freq.bar <- gather(select(freq,contIncCloseTotal,contIncNotCloseTotal,contChalCloseTotal,contChalNotCloseTotal,contOpenCloseTotal,contOpenNotCloseTotal) %>% mutate_each(funs(cumsum)) %>% summarize_each(funs(max)))
colnames(freq.bar) <- c("type","total")
freq.bar$type <- c("Incumbent","Incumbent","Challenger","Challenger","Open","Open")
freq.bar$Election <- c("Comptitive","Not-Comptitive","Comptitive","Not-Comptitive","Comptitive","Not-Comptitive")
freq.bar <- mutate(freq.bar,perc = total / sum(total))
freq.bar$donor <- "Frequent Donors"

# infrequent donors
infreq.bar <- gather(select(infreq,contIncCloseTotal,contIncNotCloseTotal,contChalCloseTotal,contChalNotCloseTotal,contOpenCloseTotal,contOpenNotCloseTotal) %>% mutate_each(funs(cumsum)) %>% summarize_each(funs(max)))
colnames(infreq.bar) <- c("type","total")
infreq.bar$type <- c("Incumbent","Incumbent","Challenger","Challenger","Open","Open")
infreq.bar$Election <- c("Comptitive","Not-Comptitive","Comptitive","Not-Comptitive","Comptitive","Not-Comptitive")
infreq.bar <- mutate(infreq.bar,perc = total / sum(total))
infreq.bar$donor <- "Infrequent Donors"


# SuperPAC donors
ie.bar <- gather(select(ie,contIncCloseTotal,contIncNotCloseTotal,contChalCloseTotal,contChalNotCloseTotal,contOpenCloseTotal,contOpenNotCloseTotal) %>% mutate_each(funs(cumsum)) %>% summarize_each(funs(max)))
colnames(ie.bar) <- c("type","total")
ie.bar$type <- c("Incumbent","Incumbent","Challenger","Challenger","Open","Open")
ie.bar$Election <- c("Comptitive","Not-Comptitive","Comptitive","Not-Comptitive","Comptitive","Not-Comptitive")
ie.bar <- mutate(freq.bar,perc = total / sum(total))
ie.bar$donor <- "SuperPAC Donors"

b <- ggplot(ie.bar,aes(x=type,y=perc,fill=Election))
b + geom_bar(stat = "identity")

# Non-SuperPAC donors
noIE.bar <- gather(select(noIE,contIncCloseTotal,contIncNotCloseTotal,contChalCloseTotal,contChalNotCloseTotal,contOpenCloseTotal,contOpenNotCloseTotal) %>% mutate_each(funs(cumsum)) %>% summarize_each(funs(max)))
colnames(noIE.bar) <- c("type","total")
noIE.bar$type <- c("Incumbent","Incumbent","Challenger","Challenger","Open","Open")
noIE.bar$Election <- c("Comptitive","Not-Comptitive","Comptitive","Not-Comptitive","Comptitive","Not-Comptitive")
noIE.bar <- mutate(noIE.bar,perc = total / sum(total))
noIE.bar$donor <- "Non-SuperPAC Donors"

bar <- bind_rows(freq.bar,infreq.bar)
bar$type <- factor(bar$type, levels = c("Incumbent","Challenger","Open"))

#plot by percentage
d <- ggplot(bar,aes(x=type,y=perc))+ geom_bar(stat = "identity", position = "stack", aes(fill=Election))
d + facet_grid(. ~ donor) + theme_minimal() +  xlab("Recipient Candidate Type") + ylab("Total Contributions (%)") + scale_y_continuous(labels = scales::percent)

#plot by total amount
e <- ggplot(bar,aes(x=type,y=total))+ geom_bar(stat = "identity", position = "stack", aes(fill=Election))
e + facet_grid(. ~ donor) + theme_minimal() + theme(legend.title=element_blank())

barIE <- bind_rows(ie.bar,noIE.bar)
barIE$type <- factor(bar$type, levels = c("Incumbent","Challenger","Open"))

#plot by percentage
d <- ggplot(barIE,aes(x=type,y=perc))+ geom_bar(stat = "identity", position = "stack", aes(fill=Election))
d + facet_grid(. ~ donor) + theme_minimal() +  xlab("Recipient Candidate Type") + ylab("Total Contributions (%)") + scale_y_continuous(labels = scales::percent)

#plot by total amount
e <- ggplot(barIE,aes(x=type,y=total))+ geom_bar(stat = "identity", position = "stack", aes(fill=Election))
e + facet_grid(. ~ donor) + theme_minimal() + theme(legend.title=element_blank())


