rm(list=ls())
   #// remove all
rm(US.stock.lm)

install.packages("cowplot")
library("ggplot2")
library("dplyr")
library("rgl")
library(cowplot)
##//import the data- 
US.stock<-read.csv("2018_Financial_Data.csv",header=TRUE)
head(US.stock.new)


##/cleaning data for each index
US.stock.roe.new<-filter(US.stock,!is.na(ROE),
                     ROE<2,ROE>-2,Sector=="Technology"|Sector=="Financial Services")

US.stock.eps.new<-filter(US.stock,!is.na(EPS),
                         EPS<25,EPS>-25,Sector=="Technology"|Sector=="Financial Services")
US.stock.ctd.new<-filter(US.stock,!is.na(cashFlowToDebtRatio),
                         cashFlowToDebtRatio<5,cashFlowToDebtRatio>-5,Sector=="Technology"|Sector=="Financial Services")           
                    
US.stock.pe.new<-filter(US.stock,!is.na(PE.ratio),
                         PE.ratio<100,Sector=="Technology"|Sector=="Financial Services")                     
US.stock.rde.new<-filter(US.stock,!is.na(R.D.Expenses),
                         R.D.Expenses>0,Sector=="Technology"|Sector=="Financial Services") 
US.stock.pri.new<-filter(US.stock,!is.na(X2019.PRICE.VAR....),
                         X2019.PRICE.VAR....<200,Sector=="Technology"|Sector=="Financial Services")
US.stock.cr.new<-filter(US.stock,!is.na(cashRatio),Sector=="Technology"|Sector=="Financial Services")
US.stock.dr.new<-filter(US.stock,!is.na(debtRatio),Sector=="Technology"|Sector=="Financial Services")
US.stock.ta.new<-filter(US.stock,!is.na(Total.assets),Sector=="Technology"|Sector=="Financial Services")
US.stock.at.new<-filter(US.stock,!is.na(assetTurnover),Sector=="Technology"|Sector=="Financial Services")
US.stock.dy.new<-filter(US.stock,!is.na(Dividend.Yield),Sector=="Technology"|Sector=="Financial Services")
US.stock.inv.new<-filter(US.stock,!is.na(Investments),Sector=="Technology"|Sector=="Financial Services")

US.stock.lmtec<-filter(US.stock,!is.na(EPS),!is.na(Investments),!is.na(R.D.Expenses),
                    ROE<10,ROE>-10,
                    EPS<25,EPS>-25,
                    Sector=="Technology",
                    R.D.Expenses<5000000000,R.D.Expenses!=0
                    )
US.stock.lmfin<-filter(US.stock,!is.na(ROE),!is.na(EPS),!is.na(cashRatio),!is.na(debtRatio),!is.na(assetTurnover),
                    ROE<10,ROE>-10,
                    EPS<25,EPS>-25,
                    Sector=="Financial Services",
                    Revenue>0,Revenue<40000000000,
                    Investments>0
)

#add column of invest and R&D index 
US.stock.lmtec<-mutate(US.stock.lmtec, rder=scale(US.stock.lmtec$R.D.Expenses,center=TRUE,scale=TRUE),intr=scale(US.stock.lmtec$Investments,center=TRUE,scale=TRUE))
US.stock.lmfin<-mutate(US.stock.lmfin, rder=scale(US.stock.lmfin$R.D.Expenses,center=TRUE,scale=TRUE),intr=scale(US.stock.lmfin$Investments,center=TRUE,scale=TRUE))
print(US.stock.lmtec)
mutate(US.stock.lmfin, intr=Investments/Total.assets)

##/ standarization

scale(US.stock.lmtec$Investments,center=TRUE,scale=TRUE)
scale(US.stock.lmtec$R.D.Expenses,center=TRUE,scale=TRUE)

#boxplot///
p.roe<-ggplot(US.stock.roe.new, aes(Sector,ROE))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='ROE boxplot' )

p.eps<-ggplot(US.stock.eps.new, aes(Sector,EPS))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='eps boxplot' )

p.ctd<-ggplot(US.stock.ctd.new, aes(Sector,cashFlowToDebtRatio))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='cashtodebt boxplot' )

p.pe<-ggplot(US.stock.pe.new, aes(Sector,PE.ratio))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='PE ratio boxplot' )

p.rde<-ggplot(US.stock.rde.new, aes(Sector,R.D.Expenses))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='R.D boxplot' )

p.pri<-ggplot(US.stock.pri.new, aes(Sector,X2019.PRICE.VAR....))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='price var boxplot' )

p.cr<-ggplot(US.stock.cr.new, aes(Sector,cashRatio))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='cashratio boxplot' )

p.dr<-ggplot(US.stock.dr.new, aes(Sector,debtRatio))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='debtratio boxplot' )

p.ta<-ggplot(US.stock.ta.new, aes(Sector,Total.assets))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='total asset boxplot' )

p.at<-ggplot(US.stock.at.new, aes(Sector,assetTurnover))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='asset turnover boxplot' )

p.dy<-ggplot(US.stock.dy.new, aes(Sector,Dividend.Yield))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='dividend yield boxplot' )

p.inv<-ggplot(US.stock.inv.new, aes(Sector,Investments))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(title='investment boxplot' )

plot_grid(p.roe,p.eps,p.rde,p.pri,p.cr,p.dr,p.at,p.inv, labels = "AUTO")

#group by sector for each index ( this does not apply in this report)
US.stock.roe<-group_by(US.stock.roe.new,Sector)
ROE_sector<-summarise(US.stock.roe,
               count=n(),
               avg_roe=mean(ROE),max_roe=max(ROE),min_roe=min(ROE))

US.stock.ctd<-group_by(US.stock.ctd.new,Sector)
ctd_sector<-summarise(US.stock.ctd,
               count=n(),
               avg_cashtodebit=mean(cashFlowToDebtRatio),max_roe=max(cashFlowToDebtRatio),min_roe=min(cashFlowToDebtRatio))
US.stock.eps<-group_by(US.stock.eps.new,Sector)
eps_sector<-summarise(US.stock.eps,
                      count=n(),
               avg_eps=mean(EPS),max_roe=max(EPS),min_roe=min(EPS))
US.stock.cr<-group_by(US.stock.cr.new,Sector)
cr_sector<-summarise(US.stock.cr,
                      count=n(),
               avg_cashratio=mean(cashRatio),max_roe=max(cashRatio),min_roe=min(cashRatio))
US.stock.dr<-group_by(US.stock.dr.new,Sector)
dr_sector<-summarise(US.stock.dr,
                     count=n(),
               avg_debt=mean(debtRatio),max_roe=max(debtRatio),min_roe=min(debtRatio))
US.stock.ta<-group_by(US.stock.ta.new,Sector)
ta_sector<-summarise(US.stock.ta,
                     count=n(),
               avg_asset=mean(Total.assets),max_roe=max(Total.assets),min_roe=min(Total.assets))
US.stock.at<-group_by(US.stock.at.new,Sector)
at_sector<-summarise(US.stock.at,
                     count=n(),
               avg_assto=mean(assetTurnover),max_roe=max(assetTurnover),min_roe=min(assetTurnover))
US.stock.dy<-group_by(US.stock.dy.new,Sector)
dy_sector<-summarise(US.stock.dy,
                     count=n(),
               avg_dvyd=mean(Dividend.Yield),max_roe=max(Dividend.Yield),min_roe=min(Dividend.Yield))
US.stock.pe<-group_by(US.stock.pe.new,Sector)
pe_sector<-summarise(US.stock.pe,
                     count=n(),
               avg_pert=mean(PE.ratio),max_roe=max(PE.ratio),min_roe=min(PE.ratio))
US.stock.rde<-group_by(US.stock.rde.new,Sector)
rde_sector<-summarise(US.stock.rde,
                     count=n(),
               avg_R.D=mean(R.D.Expenses),max_R.D=max(R.D.Expenses),min_R.D=min(R.D.Expenses))
US.stock.pri<-group_by(US.stock.pri.new,Sector)
pri_sector<-summarise(US.stock.pri,
                      count=n(),
               avg_Price=mean(X2019.PRICE.VAR....),max_Price=max(X2019.PRICE.VAR....),min_Price=min(X2019.PRICE.VAR....))

US.stock.inv<-group_by(US.stock.inv.new,Sector)
inv_sector<-summarise(US.stock.inv,
                      count=n(),
                      avg_inv=mean(Investments),max_inv=max(Investments),min_inv=min(Investments))
ind_list<-Reduce(function(...) merge(..., all=TRUE),
       list(ROE_sector, eps_sector, cr_sector,dr_sector,ta_sector,at_sector,dy_sector,pe_sector,rde_sector,pri_sector))



##//summary of sector
sector<-c(US.stock$Sector)
ggplot(data.frame(sector), aes(x=sector)) +
  geom_bar(width = 0.8)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+labs(x="Sector", y="Count",
                  title='Sector summary', 
                  colour="Sector",
                  caption='2018 US.stock')

##/linear regression-tec

model_tec <- lm(X2019.PRICE.VAR.... ~ EPS+intr+rder, data = US.stock.lmtec)
model_tec <- lm(X2019.PRICE.VAR.... ~ EPS + ROE +cashRatio+debtRatio+assetTurnover, data = US.stock.lmtec)
summary(model_tec)
confint(model_tec)
model_tec <- lm(Revenue ~ Investments, data = US.stock.lmtec)
chisq.test(US.stock.lmtec$Revenue,y=US.stock.lmtec$R.D.Expenses)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$R.D.Expenses)

plot(model_tec)

EPS + ROE +cashRatio+debtRatio+assetTurnover

##/linear regression-fin
model_fin <- lm(X2019.PRICE.VAR.... ~ EPS+ROE+cashRatio++debtRatio+assetTurnover, data = US.stock.lmfin)
model_fin <- lm(X2019.PRICE.VAR.... ~ EPS+intr+rder, data = US.stock.lmfin)
summary(model_fin)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$Investments)
confint(model_fin)
plot(model_fin)




# indicators correlations
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$ROE)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$EPS)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$cashRatio)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$debtRatio)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$assetTurnover)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$Investments)
cor.test(US.stock.lmfin$Revenue, US.stock.lmfin$R.D.Expenses)

cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$ROE)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$EPS)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$cashRatio)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$debtRatio)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$assetTurnover)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$Investments)
cor.test(US.stock.lmtec$Revenue, US.stock.lmtec$R.D.Expenses)


# plot corrle
plot(US.stock.lmtec$Revenue ~ US.stock.lmtec$R.D.Expenses, data = US.stock.lmtec)
plot(US.stock.lmfin$Revenue ~ US.stock.lmfin$Investments, data = US.stock.lmfin)
ggplot(
  data=US.stock.lmfin, 
  aes(x=Revenue, y=Investments))+
  geom_point()+labs(x="Revenue", y="Investment",
                   title='Investment correlation', 
                   colour="Sector",
                   caption='2018 US.stock')
ggplot(
  data=US.stock.lmtec, 
  aes(x=Revenue, y=R.D.Expenses))+
  geom_point()+geom_point()+labs(x="Revenue", y="R.D.Expenses",
                                 title='R.D.Expenses correlation', 
                                 colour="Sector",
                                 caption='2018 US.stock')
