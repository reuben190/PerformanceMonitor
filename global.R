load("DTR012015To052017.RData")
library(data.table)
library(lubridate)
library(dygraphs)

GroupBy=list("State"=1,"Program Region"=2,"County"=3,"Service Group"=4,
             "Lender"=5,"Appraiser"=6,"Market"=7,"Field Agent Skill"=8,
             "Area Type"=9,"Region Manager"=10,"Appraisal Purpose"=11)
GroupBy_Actual=list("State"=1,"Region"=2,"County"=3,"Service Class"=4,
                    "Lender Corporate Name"=5,"Appraiser"=6,"Market Type"=7,"Skill Type"=8,
                    "Area Type"=9,"Region Manager"=10,"Appraisal Purpose"=11)

DTRData=DTRData[,.(
  ##Order Info
  `Order ID`,
  `Order #`,
  State,
  Region,
  County,
  `Service Group`=`Service Class`,
  `Lender Corporate Name`,
  Appraiser,
  `Market Type`,
  `Skill Type`,
  `Area Type`,
  `Region Manager`,
  `Appraisal Purpose`,
  `Cancel Date`,
  `Complete Date`,
  `Create Date`,
  `Report Upload Date`,
  `First Deliver Date`,
  `Months`,
  
  ##Order Stats
  `Final Invoice Amount`,
  `Appraiser's Fee`,
  `Management Fee`,
  Performance,
  InspectTAT=as.numeric(difftime(`Inspection Date`,`Create Date`,units = "days")),
  UploadTAT=as.numeric(difftime(`Report Upload Date`,`Create Date`,units = "days")),
  `1stDeliveryTAT`=as.numeric(difftime(`First Deliver Date`,`Create Date`,units = "days")),
  Quality=`Solidifi Quality Score`/1000)]
