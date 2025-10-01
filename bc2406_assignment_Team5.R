##########################################################
# Libraries
##########################################################
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(corrplot)

##########################################################
# Data Import & Cleaning
##########################################################
lawsuit.dt <- fread("Lawsuit.csv")

lawsuit.dt[, Dept  := factor(Dept, levels = 1:6,
                             labels = c("Biochemistry/Molecular Biology",
                                        "Physiology",
                                        "Genetics",
                                        "Pediatrics",
                                        "Medicine",
                                        "Surgery"))]
lawsuit.dt[, Clin  := factor(Clin, levels = c(0,1), labels = c("Research","Clinical"))]
lawsuit.dt[, Cert  := factor(Cert, levels = c(0,1), labels = c("Not Certified","Board Certified"))]
lawsuit.dt[, Gender:= factor(Gender, levels = c(0,1), labels = c("Female","Male"))]
lawsuit.dt[, Rank  := factor(Rank, levels = c(1,2,3), labels = c("Assistant","Associate","Full"))]

## Dataset excluding Surgery
lawsuit2.dt <- lawsuit.dt[Dept != "Surgery"]

## Experience bins
bins   <- c(0, 5, 10, 15, 20, 25, 30, 40, 50)
labels <- c("0-5","6-10","11-15","16-20","21-25","26-30","31-40","41-50")
lawsuit.dt[, Exp_Bin := cut(Exper, breaks = bins, labels = labels, right = TRUE, include.lowest = TRUE)]
lawsuit2.dt[, Exp_Bin := cut(Exper, breaks = bins, labels = labels, right = TRUE, include.lowest = TRUE)]


##########################################################
# SLIDE 2: Gender proportion, Correlations, Exp vs Salary
##########################################################

## Gender proportion
gender_counts <- lawsuit.dt[, .N, by = Gender][, Proportion := N/sum(N)*100]
gender_counts[, Label := paste0(round(Proportion,1),"%")]

ggplot(gender_counts, aes(x = "", y = Proportion, fill = Gender)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  coord_polar(theta = "y") +
  labs(title = "Overall Gender Proportion") +
  scale_fill_manual(values = c("Female" = "lightcoral","Male" = "steelblue")) +
  theme_void()

## Correlation Heatmap
corr_data <- lawsuit.dt[, .(Gender = as.numeric(Gender=="Male"),
                            Clin   = as.numeric(Clin=="Clinical"),
                            Cert   = as.numeric(Cert=="Board Certified"),
                            Rank   = as.numeric(Rank),
                            Prate, Exper, Sal94, Sal95)]
corr_matrix <- cor(corr_data, use="complete.obs")
corr_long   <- as.data.frame(as.table(corr_matrix))

ggplot(corr_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color="white") +
  geom_text(aes(label=sprintf("%.2f", Freq)), color="black", size=4) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Correlation Heatmap (Including Surgery)", fill="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

## Experience vs Salary (Gender)
ggplot(lawsuit.dt, aes(x=Exper, y=Sal95, color=Gender)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Experience vs Salary by Gender (Including Surgery)",
       x="Years of Experience", y="Salary (1995)", color="Gender") +
  scale_y_continuous(labels=comma) +
  scale_color_manual(values=c("Female"="red","Male"="blue")) +
  theme_minimal()

## Gender Proportion by Experience Bin
exp_gender_counts <- lawsuit.dt[, .N, by=.(Exp_Bin, Gender)][, Percent := N/sum(N)*100, by=Exp_Bin]

ggplot(exp_gender_counts, aes(x=Exp_Bin, y=Percent, fill=Gender)) +
  geom_col(position="stack") +
  labs(title="Gender Proportion by Experience (Including Surgery)",
       x="Experience Bin", y="Percentage of Faculty") +
  theme_minimal() +
  scale_fill_manual(values=c("Female"="lightcoral","Male"="skyblue")) +
  theme(axis.text.x = element_text(angle=45,hjust=1))


##########################################################
# SLIDE 3: Department-level Analysis & Surgery Exclusion
##########################################################

## Salary distribution by department
ggplot(lawsuit.dt, aes(x=Dept, y=Sal95)) +
  geom_boxplot(fill="skyblue") +
  labs(title="Salary Distribution by Department (Including Surgery)",
       x="Department", y="Salary (1995)") +
  scale_y_continuous(labels=comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1))

## Gender proportion by department
dept_gender <- lawsuit.dt[, .N, by=.(Dept, Gender)][, Proportion := N/sum(N)*100, by=Dept]

ggplot(dept_gender, aes(x=Dept, y=Proportion, fill=Gender)) +
  geom_col(position="stack") +
  labs(title="Proportion of Male/Female by Department (Including Surgery)",
       y="Percentage (%)") +
  scale_y_continuous(labels=function(x) paste0(round(x),"%")) +
  scale_fill_manual(values=c("Female"="lightcoral","Male"="steelblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1))

## Salary comparison by gender
ggplot(lawsuit.dt, aes(x=Gender, y=Sal95, fill=Gender)) +
  geom_boxplot() +
  labs(title="Salary Comparison by Gender (Including Surgery)",
       x="Gender", y="Salary (1995)") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("Female"="lightcoral","Male"="steelblue")) +
  theme_minimal()

## Salary comparison (Excluding Surgery)
ggplot(lawsuit2.dt, aes(x=Gender, y=Sal95, fill=Gender)) +
  geom_boxplot() +
  labs(title="Salary Comparison by Gender (Surgery Excluded)",
       x="Gender", y="Salary (1995)") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("Female"="lightcoral","Male"="steelblue")) +
  theme_minimal()


##########################################################
# SLIDE 4: Experience, Salary & Rank Analysis (Excl. Surgery)
##########################################################

## Salary vs Experience bins (Clinical only)
lawsuit2.dt[, exp_bin := cut(Exper, breaks=quantile(Exper, probs=seq(0,1,.1), na.rm=TRUE),
                             include.lowest=TRUE)]
means <- lawsuit2.dt[Clin=="Clinical", .(mean_sal=mean(Sal95,na.rm=TRUE)), by=.(exp_bin, Gender)]

ggplot(means, aes(x=exp_bin, y=mean_sal, group=Gender, color=Gender)) +
  geom_line(linewidth=1.1) + geom_point(size=2) +
  labs(title="Average Salary by Experience Bins (Clinical)",
       x="Experience (deciles)", y="Mean Salary") +
  theme_minimal(base_size=14) +
  theme(axis.text.x=element_text(angle=20,hjust=1))

## Salary distribution by rank & gender (Excl. Surgery)
ggplot(lawsuit2.dt, aes(x=Rank, y=Sal94, fill=Gender)) +
  geom_violin(alpha=0.6, position=position_dodge(width=0.8)) +
  geom_boxplot(width=0.2, position=position_dodge(width=0.8)) +
  labs(title="Salary Distribution by Rank and Gender (Excluding Surgery Dept)",
       x="Rank", y="Salary (1995)") +
  scale_fill_manual(values=c("Female"="lightcoral","Male"="#00BFC4")) +
  scale_y_continuous(labels=dollar) +
  theme_minimal(base_size=14)

## Experience vs Salary (Excl. Surgery)
ggplot(lawsuit2.dt, aes(x=Exper, y=Sal95, color=Gender)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Experience vs Salary by Gender (Surgery Excluded)",
       x="Years of Experience", y="Salary (1995)", color="Gender") +
  scale_y_continuous(labels=comma) +
  scale_color_manual(values=c("Female"="red","Male"="blue")) +
  theme_minimal()


##########################################################
# SLIDE 5: Salary by Clinical/Certification
##########################################################

## Salary by Clinical track
pal <- c(Female="#FFC5D3", Male="#62AEC5")

ggplot(lawsuit2.dt, aes(x=Gender, y=Sal94, fill=Gender)) +
  geom_boxplot(color="grey20") +
  facet_wrap(~Clin) +
  scale_fill_manual(values=pal) +
  labs(y="Salary (1994)", x=NULL) +
  theme_minimal(base_size=14) +
  theme(legend.position="none",
        strip.background=element_rect(fill="#F3F4F6", color=NA),
        strip.text=element_text(face="bold"))

## Salary by Board Certification
sal_cert <- lawsuit2.dt[, .(mean_sal=mean(Sal94,na.rm=TRUE),
                            se=sd(Sal94,na.rm=TRUE)/sqrt(.N)), by=.(Cert, Gender)]

ggplot(sal_cert, aes(x=Cert, y=mean_sal, fill=Gender)) +
  geom_col(position=position_dodge(width=0.7), width=0.6) +
  geom_errorbar(aes(ymin=mean_sal-se, ymax=mean_sal+se),
                position=position_dodge(width=0.7), width=0.2) +
  labs(title="Average Salary by Board Certification and Gender",
       x="Board Certification", y="Mean Salary (1994)") +
  scale_fill_manual(values=pal) +
  theme_minimal(base_size=14)


##########################################################
# SLIDE 6: Rank vs Gender & Career Progression
##########################################################

## Rank distribution within departments by gender
ggplot(lawsuit.dt, aes(x=Dept, fill=Rank)) +
  geom_bar(position="fill") +
  facet_wrap(~Gender) +
  labs(title="Rank Distribution within Departments by Gender",
       x="Department", y="Proportion within Department", fill="Rank") +
  scale_fill_manual(values=c("Assistant"="lightblue","Associate"="steelblue","Full"="darkblue")) +
  scale_y_continuous(labels=percent)

##########################################################
# SLIDE 7: Rank Proportions by Gender & Clinical Track
##########################################################

## Proportion of faculty rank by gender

ggplot(lawsuit.dt, aes(x=Gender, fill=Rank)) +
  geom_bar(position="fill") +
  facet_wrap(~Clin) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Proportion of Faculty Rank by Gender",
       x="Gender",
       y="Proportion (%)",
       fill="Rank") +
  theme_minimal() 

# Summarise proportion of Full Professors
prof_summary <- lawsuit.dt %>%
  group_by(Clin, Gender) %>%
  summarise(full_prof_rate = mean(Rank == "Full"), .groups="drop")

ggplot(prof_summary, aes(x=Clin, y=full_prof_rate, fill=Gender)) +
  geom_col(position="dodge") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Probability of Being a Full Professor by Gender and Rank",
       x="Rank",
       y="% Full Professors",
       fill="Gender") +
  theme_minimal()



##########################################################
# SLIDE 8: Career Progression: Experience vs Rank by Gender
##########################################################

## Career progression (Experience vs Rank by Gender)
set.seed(123)
ggplot(lawsuit.dt, aes(x=Exper, y=as.numeric(Rank), color=Gender)) +
  geom_jitter(width=0.2, height=0.2) +
  facet_wrap(~Clin) +
  labs(title="Experience vs Rank by Gender",
       x="Years of Experience", y="Rank (1=Assistant, 2=Associate, 3=Full)", color="Gender") +
  scale_y_continuous(breaks=1:3, labels=levels(lawsuit.dt$Rank)) +
  scale_color_manual(values = c(Female = "red", Male = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Clinical only
# if Clin is a factor with labels:
clin_label <- if (is.factor(lawsuit.dt$Clin)) "Clinical" else 1

set.seed(123)
ggplot(subset(lawsuit.dt, Clin == clin_label),
       aes(x = Exper, y = as.numeric(Rank), color = Gender)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Experience vs Rank by Gender (Clinical Emphasis)",
       x = "Years of Experience", y = "Rank") +
  scale_y_continuous(breaks = 1:3, labels = levels(lawsuit.dt$Rank)) +
  scale_color_manual(values = c(Female = "red", Male = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#Research only
res_label <- if (is.factor(lawsuit.dt$Clin)) "Research" else 0

set.seed(123)
ggplot(subset(lawsuit.dt, Clin == res_label),
       aes(x = Exper, y = as.numeric(Rank), color = Gender)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Experience vs Rank by Gender (Research Emphasis)",
       x = "Years of Experience", y = "Rank") +
  scale_y_continuous(breaks = 1:3, labels = levels(lawsuit.dt$Rank)) +
  scale_color_manual(values = c(Female = "red", Male = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


##########################################################
# SLIDE 9: Publications vs Rank (by Gender & Clinical)
##########################################################
##  Publication Rate vs Rank by Gender
ggplot(lawsuit.dt, aes(x=Prate, y=as.numeric(Rank), color=Gender)) +
  geom_jitter(alpha=0.6, width=0, height=0.1) +
  stat_smooth(method="lm", se=FALSE) +
  scale_y_continuous(breaks=1:3, labels=levels(lawsuit.dt$Rank)) +
  labs(title="Publication Rate vs Rank by Gender (by Clinical Emphasis)",
       x="Publication Rate", y="Rank", color="Gender") +
  facet_wrap(~Clin)

##  Publication Rate vs Rank (Research only)
ggplot(lawsuit.dt[Clin=="Research"], aes(x=Rank, y=Prate, fill=Gender)) +
  geom_boxplot(alpha=0.7, outlier.alpha=0.4) +
  labs(title="Publication Rate vs Rank by Gender (Research Faculty Only)",
       x="Rank", y="Publication Rate", fill="Gender")

## [slide 9] Publication Rate vs Rank (Clinical only)
ggplot(lawsuit.dt[Clin=="Clinical"], aes(x=Rank, y=Prate, fill=Gender)) +
  geom_boxplot(alpha=0.7, outlier.alpha=0.4) +
  labs(title="Publication Rate vs Rank by Gender (Clinical Faculty Only)",
       x="Rank", y="Publication Rate", fill="Gender")


