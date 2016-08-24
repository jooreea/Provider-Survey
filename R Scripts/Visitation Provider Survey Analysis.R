library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

vsp1<- read.csv("S:/Data Portal/Jooree/finsurvey/Data Analysis & Reports/Region1ProviderSurve_DATA_2016-08-24_1017.csv", na.strings = '')
vsp2<- read.csv("S:/Data Portal/Jooree/finsurvey/Data Analysis & Reports/Region1ProviderSurve_DATA_2016-08-24_1018.csv", na.strings = '')

#Filter respondents from each of the two datasets from REDCap projects, removing identifying information
vsp1<- filter(vsp1, consent==1)%>%
  select(organizational_role___1:auth_lapse_i___5)
  
vsp2<- filter(vsp2, consent==1)%>%
  select(organizational_role___1:auth_lapse_i___5)

#Join them together
vsp<-bind_rows(vsp1, vsp2)

#There are a total of 36 respondents from visitation provider organizations in Region 1

#Select and manipulate Data for Organizational Role Analysis
organizational_role<- select(vsp, organizational_role___1:organizational_role___6)%>%
  summarise_each(funs(sum))%>%
  melt()%>%
  mutate(percent=paste(round(value/sum(value)*100, 2), "%", sep=""))

#Plot bar graph of respondents in each organizational role
ggplot(organizational_role, aes(variable, value, fill=variable))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Organizational Role and Contact with Clients",
                      labels=c("Administrator/Manager: Minimal Contact", 
                               "Clerical Support: Minimal Contact", 
                               "Visit Scheduler: Frequent Contact",
                               "Visit Supervisor: Frequent Contact", 
                               "Other Role: Minimal Contact",
                               "Other Role: Frequent Contact"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(organizational_role___1="Admin/Manager", 
                            organizational_role___2="Clerical Support", 
                            organizational_role___3="Visit Scheduler", 
                            organizational_role___4="Visit Supervisor", 
                            organizational_role___5="Other: Minimal", 
                            organizational_role___6="Other: Frequent"),
                   drop=FALSE)+
  ggtitle("Organizational Role")+
  labs(x="Organizational Role", y="Count")+
  geom_text(aes(label=paste(value), vjust= 0))+
  geom_text(aes(label=paste(percent), vjust=2))+
  theme_bw()

#Select and manipulate data for performance measure importance ranking analysis

performance_measure<- select(vsp, days_to_schedule:visit_frequency)%>%
  melt()%>%
  unite(Measure_Rank, variable:value)%>%
  group_by(Measure_Rank)%>%
  mutate(Count=n_distinct(Measure_Rank))%>%
  group_by(Measure_Rank)%>%
  mutate(Count=sum(Count))%>%
  distinct(Measure_Rank)
 

#Plot bar graph of importance of Days to Schedule

days_to_schedule<- performance_measure[1:5,]
  
ggplot(days_to_schedule, aes(Measure_Rank, Count, fill=Measure_Rank))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Importance Ranking",
                      labels=c("Most Important", 
                               "More Important", 
                               "Less Important",
                               "Least Important", 
                               "NA"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(days_to_schedule_1="Most Important", 
                            days_to_schedule_2="More Important", 
                            days_to_schedule_3="Less Important", 
                            days_to_schedule_4="Least Important", 
                            days_to_schedule_NA="NA"),
                   drop=FALSE)+
  ggtitle("Organizational Importance of Days To Schedule")+
  labs(x="Importance Ranking", y="Count")+
  geom_text(aes(label=paste(Count), vjust=0))+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Plot bar graph of importance of Days to First Visit

days_to_first_visit<- performance_measure[6:10,]

ggplot(days_to_first_visit, aes(Measure_Rank, Count, fill=Measure_Rank))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Importance Ranking",
                      labels=c("Most Important", 
                               "More Important", 
                               "Less Important",
                               "Least Important", 
                               "NA"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(days_to_first_visit_1="Most Important", 
                            days_to_first_visit_2="More Important", 
                            days_to_first_visit_3="Less Important", 
                            days_to_first_visit_4="Least Important", 
                            days_to_first_visit_NA="NA"),
                   drop=FALSE)+
  ggtitle("Organizational Importance of Days to First Visit")+
  labs(x="Importance Ranking", y="Count")+
  geom_text(aes(label=paste(Count), vjust=0))+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Plot bar graph of importance of Visit Regularity

visit_regularity<- performance_measure[11:14,]

ggplot(visit_regularity, aes(Measure_Rank, Count, fill=Measure_Rank))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Importance Ranking",
                      labels=c("Most Important", 
                               "More Important", 
                               "Less Important",
                               "Least Important"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(visit_regularity_1="Most Important", 
                            visit_regularity_2="More Important", 
                            visit_regularity_3="Less Important", 
                            visit_regularity_4="Least Important"),
                   drop=FALSE)+
  ggtitle("Organizational Importance of Visit Regularity")+
  labs(x="Importance Ranking", y="Count")+
  geom_text(aes(label=paste(Count), vjust=0))+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Plot bar graph of importance of Visit Frequency

visit_frequency<- performance_measure[15:19,]

ggplot(visit_frequency, aes(Measure_Rank, Count, fill=Measure_Rank))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Importance Ranking",
                      labels=c("Most Important", 
                               "More Important", 
                               "Less Important",
                               "Least Important", 
                               "NA"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(visit_frequency_1="Most Important", 
                            visit_frequency_2="More Important", 
                            visit_frequency_3="Less Important", 
                            visit_frequency_4="Least Important", 
                            visit_frequency_NA="NA"),
                   drop=FALSE)+
  ggtitle("Organizational Importance of Visit Frequency")+
  labs(x="Importance Ranking", y="Count")+
  geom_text(aes(label=paste(Count), vjust=0))+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Select and manipulate data for organizational control analysis

organizational_control<- select(vsp, days_to_schedule_b:visit_frequency_b)%>%
  melt()%>%
  unite(Measure_Control, variable:value)%>%
  group_by(Measure_Control)%>%
  mutate(Count=n_distinct(Measure_Control))%>%
  group_by(Measure_Control)%>%
  mutate(Count=sum(Count))%>%
  distinct(Measure_Control)


#Plot bar graph of level of control over Days to Schedule
days_to_schedule_control<- organizational_control[1:5,]

ggplot(days_to_schedule_control, aes(Measure_Control, Count, fill=Measure_Control))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Level of Control",
                      labels=c("High Control", 
                               "Moderate Control", 
                               "Low Control",
                               "No Control", 
                               "NA"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(days_to_schedule_b_1="High Control", 
                            days_to_schedule_b_2="Moderate Control", 
                            days_to_schedule_b_3="Low Control", 
                            days_to_schedule_b_4="No Control", 
                            days_to_schedule_b_NA="NA"),
                   drop=FALSE)+
  ggtitle("Organizational Control over Days to Schedule")+
  labs(x="Level of Control", y="Count")+
  geom_text(aes(label=paste(Count), vjust=0))+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Plot bar graph of level of control over Days to First Visit
days_to_first_visit_control<- organizational_control[6:10,]

ggplot(days_to_first_visit_control, aes(Measure_Control, Count, fill=Measure_Control))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Level of Control",
                      labels=c("High Control", 
                               "Moderate Control", 
                               "Low Control",
                               "No Control", 
                               "NA"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(days_to_first_visit_b_1="High Control", 
                            days_to_first_visit_b_2="Moderate Control", 
                            days_to_first_visit_b_3="Low Control", 
                            days_to_first_visit_b_4="No Control", 
                            days_to_first_visit_b_NA="NA"),
                   drop=FALSE)+
  ggtitle("Organizational Control over Days to First Visit")+
  labs(x="Level of Control", y="Count")+
  geom_text(aes(label=paste(Count)), vjust=0)+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Plot bar graph of level of control over Visit Regularity
visit_regularity_control<- organizational_control[11:14,]

ggplot(visit_regularity_control, aes(Measure_Control, Count, fill=Measure_Control))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Level of Control",
                      labels=c("High Control", 
                               "Moderate Control", 
                               "Low Control",
                               "No Control"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(visit_regularity_b_1="High Control", 
                            visit_regularity_b_2="Moderate Control", 
                            visit_regularity_b_3="Low Control", 
                            visit_regularity_b_4="No Control"),
                   drop=FALSE)+
  ggtitle("Organizational Control over Visit Regularity")+
  labs(x="Level of Control", y="Count")+
  geom_text(aes(label=paste(Count)), vjust=0)+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Plot bar graph of level of control over Visit Frequency
visit_frequency_control<- organizational_control[15:19,]

ggplot(visit_frequency_control, aes(Measure_Control, Count, fill=Measure_Control))+
  geom_bar(stat = "identity")+
  scale_fill_discrete(name= "Level of Control",
                      labels=c("High Control", 
                               "Moderate Control", 
                               "Low Control",
                               "No Control", 
                               "NA"),
                      drop=FALSE)+
  scale_x_discrete(labels=c(visit_frequency_b_1="High Control", 
                            visit_frequency_b_2="Moderate Control", 
                            visit_frequency_b_3="Low Control", 
                            visit_frequency_b_4="No Control", 
                            visit_frequency_b_NA="NA"),
                   drop=FALSE)+
  ggtitle("Organizational Control over Visit Frequency")+
  labs(x="Level of Control", y="Count")+
  geom_text(aes(label=paste(Count)), vjust=0)+
  geom_text(aes(label=paste(round(Count/sum(Count)*100, 2), "%", sep=""), vjust=2))+
  theme_bw()

#Frequency of Issues

# fill in missing factor levels for frequency items (not_open:auth_lapse)
vsp$not_open<- factor(vsp$not_open, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))
vsp$no_staff<- factor(vsp$no_staff, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                         
vsp$conflicting_reqs<- factor(vsp$conflicting_reqs, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))
vsp$party_disagreement<- factor(vsp$party_disagreement, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                
vsp$nap_and_feeding_times<- factor(vsp$nap_and_feeding_times, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))             
vsp$incarceration_times<- factor(vsp$incarceration_times, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))               
vsp$inconsistent_with_court<- factor(vsp$inconsistent_with_court, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))           
vsp$provider_schedule<- factor(vsp$provider_schedule, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                 
vsp$late_re_referral<- factor(vsp$late_re_referral, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                  
vsp$incorrect_number<- factor(vsp$incorrect_number, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                  
vsp$social_worker_unavailable<- factor(vsp$social_worker_unavailable, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))         
vsp$bad_famlink_information<- factor(vsp$bad_famlink_information, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))           
vsp$parties_unavailable<- factor(vsp$parties_unavailable, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))               
vsp$referral_inaccurate<- factor(vsp$referral_inaccurate, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))              
vsp$therapy_approval_delay<- factor(vsp$therapy_approval_delay, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))            
vsp$special_seat_required<- factor(vsp$special_seat_required, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))             
vsp$med_staff_needed<- factor(vsp$med_staff_needed, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                  
vsp$background_check<- factor(vsp$background_check, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                  
vsp$foster_parent_control<- factor(vsp$foster_parent_control, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))            
vsp$conflicts_ofnterest<- factor(vsp$conflicts_ofnterest, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))             
vsp$many_visit_per_ref<- factor(vsp$many_visit_per_ref, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))               
vsp$social_worker_hold<- factor(vsp$social_worker_hold, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))              
vsp$placement_changes<- factor(vsp$placement_changes, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))                
vsp$auth_lapse<- factor(vsp$auth_lapse, levels= c(1, 2, 3, 4, 5, 6, NA), labels=c("<1%", "1%-24%", "25%-50%", "50%-75%", "76%-99%", ">99%"))   

#rename frequency items

frequency<- select(vsp, not_open:auth_lapse)%>%
  rename("My organization is not open during the requested visitation hours"=not_open,
         "My organization does not have staff during the requested visitation hours"=no_staff,
         "Visit referrals have conflicting requirements (e.g. different foster parent work schedules across placements)"=conflicting_reqs,
         "Visit parties (e.g. foster parents, birth parents) disagree about the proposed visitation schedule"=party_disagreement,
         "Scheduling around naps and feeding times is difficult"=nap_and_feeding_times,
         "Scheduling around incarceration visit times is difficult"=incarceration_times,
         "Requested schedule is not consistent with the court-ordered visitation schedule"=inconsistent_with_court,
         "Providers need to be included in some portion of visitation sessions"=provider_schedule,
         "A re-referral is sent to my organization after the original referral has already expired and been removed from the schedule"=late_re_referral,
         "Referral has incorrect phone number(s) for parties"=incorrect_number,
         "Social worker won't return calls"=social_worker_unavailable,
         "Famlink information in referral is out-of-date or inaccurate"=bad_famlink_information,
         "Parties will not return call"=parties_unavailable,
         "Referral does not accurately describe complex issues"=referral_inaccurate,
         "Therapy approval is delayed"=therapy_approval_delay,
         "Special car seat is required for a child"=special_seat_required,
         "Nurse or medical staff required for medical or behavioral needs"=med_staff_needed,
         "Background check delays create difficulty in hiring new staff"=background_check,
         "Foster parents want to 'control' cases"=foster_parent_control,
         "Conflicts of interests with staff"=conflicts_ofnterest,
         "Multiple visits are made under a single referral"=many_visit_per_ref,
         "Social worker asks for a hold on a referral"=social_worker_hold,
         "Placement changes from original referral"=placement_changes,
         "Lapse of visitation authorization"=auth_lapse)
  

#Loop Function for Frequency Items

freq_function <-function(df){
  nm<- names(df)
  for(i in seq_along(nm)){
    plot <- ggplot(df, aes(x=df[[i]], fill=df[[i]]))+
      geom_bar()+
      scale_fill_discrete(name= "Issue Frequency:\nThis problem...",
                          labels=c("never happens", 
                                   "happens, but is rare", 
                                   "happens frequently, but not in all referrals",
                                   "happens in the majority of referrals", 
                                   "happens in almost every referral",
                                   "happens in all referrals"),
                          drop=FALSE)+
      scale_x_discrete(drop=FALSE)+
      ggtitle(paste(nm[i]))+
      labs(x="Issue Frequency", y="Count")
      #+geom_text(aes(label=paste(count(df[[i]])), vjust= 0))
    print(plot)
  }
}

freq_function(frequency)

#Impact

impact<-select(vsp, no_staff_i___1:auth_lapse_i___5)%>%
  melt()%>%
  group_by(variable)%>%
  summarise_each(funs(sum))

impact$variable<- as.character(impact$variable)
impact$variable<-substr(impact$variable, 1, nchar(impact$variable)-6)

impact<-spread(impact, key=variable, value=value)

measure<- as.factor(rep(c(1:5), 23))
Measure<- as.data.frame(measure)

impact<- bind_cols(Measure, impact)

impact[is.na(impact)]<-0
  
Impact<-group_by(impact, measure)%>%
  summarise_each(funs(sum))

#Rename impact items
Impact<- rename(Impact, 
                "My organization does not have staff during the requested visitation hours"=no_staff,
                "Visit referrals have conflicting requirements (e.g. different foster parent work schedules across placements)"=conflicting_reqs,
                "Visit parties (e.g. foster parents, birth parents) disagree about the proposed visitation schedule"=party_disagreement,
                "Scheduling around naps and feeding times is difficult"=nap_and_feeding_times,
                "Scheduling around incarceration visit times is difficult"=incarceration_times,
                "Requested schedule is not consistent with the court-ordered visitation schedule"=inconsist_with_court,
                "Providers need to be included in some portion of visitation sessions"=provider_schedule,
                "A re-referral is sent to my organization after the original referral has already expired and been removed from the schedule"=late_re_referral,
                "Referral has incorrect phone number(s) for parties"=incorrect_number,
                "Social worker won't return calls"=social_wk_unavailable,
                "Famlink information in referral is out-of-date or inaccurate"=bad_fl_information,
                "Parties will not return call"=parties_unavailable,
                "Referral does not accurately describe complex issues"=referral_inaccurate,
                "Therapy approval is delayed"=therapy_appr_delay,
                "Special car seat is required for a child"=special_seat_required,
                "Nurse or medical staff required for medical or behavioral needs"=med_staff_needed,
                "Background check delays create difficulty in hiring new staff"=background_check,
                "Foster parents want to 'control' cases"=foster_parent_control,
                "Conflicts of interests with staff"=conflicts_of_interest,
                "Multiple visits are made under a single referral"=many_visit_per_ref,
                "Social worker asks for a hold on a referral"=social_worker_hold,
                "Placement changes from original referral"=placement_changes,
                "Lapse of visitation authorization"=auth_lapse)

#Loop Function for Impact Items

impact_function <-function(df){
  
  for(i in 2:ncol(df)){
    plot <- ggplot(df, aes(x=df[[1]], y=df[[i]], fill=df[[1]]))+
      geom_bar(stat = "identity")+
      scale_fill_discrete(name= "Issue Impact:\nThis problem directly impacts...",
                          labels=c("Days to Schedule", 
                                   "Days to First Visit", 
                                   "Visit Regularity",
                                   "Visit Frequency", 
                                   "None of the Metrics"),
                          drop=FALSE)+
      ggtitle(paste(names(df)[[i]]))+
      labs(x="Issue Impact", y="Count") +
      theme_bw()
    print(plot)

  }
}

impact_function(Impact)



