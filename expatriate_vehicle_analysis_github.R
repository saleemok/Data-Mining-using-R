library(rvest)
library(purrr)
library(pbapply)
library(stringr)



############### extract all URLs ################

url_base <- "https://www.expatriates.com/classifieds/riy/vehicles/index.html"
webpage <- read_html(url_base)
webpage <- html_text(html_nodes(webpage,".pagination a"))
webpage <- str_replace(webpage,'Next','0')
webpage <- as.numeric(webpage)
webpage <- max(webpage)

urls <- paste0('https://www.expatriates.com/classifieds/riy/vehicles/index','.html')

for(i in 2:webpage){
  
  urls[i]<- paste0('https://www.expatriates.com/classifieds/riy/vehicles/index',i*100-100,'.html')
  
}

####### function for getting url of each post ############

 classified_urls <- function(url) {
    
    
   webpages<- c()
   
    a <- 0
    
    for(i in url){
     
      a = a+1
      webpages[[a]]<-  read_html(i)
      webpages[[a]] <-html_nodes(webpages[[a]],".listing-content a")%>% html_attr('href')
      webpages[[a]] <- str_subset(webpages[[a]],'/cls/')
      webpages[[a]] <- paste0('https://www.expatriates.com',webpages[[a]])
      
      
      
    }
   
    
    
    return(webpages)
    Sys.sleep(5)
 }
  

data <- unlist(pblapply(urls, classified_urls))

#below command is used for invoking all classified_urls, it will take more then hour to finish so we will manual select first 1500,it will finish within ~30 mint depend of internet speed
#mydata <- data

mydata <- data[1:1500]

#write.csv(data,file = 'classified_urls.csv')

####### classified description  ############



  
  
   classified_data <- function(i) {
  
    
    
    
   out <-  tryCatch(
      {
        # Just to highlight: if you want to use more than one 
        # R expression in the "try" part then you'll have to 
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression 
        # in case the "try" part was completed successfully
        
        #message("This is the 'try' part")
        
        classified_url<- read_html(i)
        
        heading <-html_text(html_nodes(classified_url,"h1"))
        
        #vehicle price
        price <- str_replace(heading,'/',',')
       
         
        
        price <- unlist(str_split(price,'[,]'))
        price <- gsub("SAR", "", price[1])
        price  <- trimws(price,'b')
        price  <- as.numeric(price)
        
         classified_detail <-html_text(html_nodes(classified_url,".no-bullet li"))
        #classified date
          
       
         
         
          
        classfdate <- sub(".*:", "", classified_detail[1])
        
        
        if(grepl('MAIL',classfdate) || grepl('mail',classfdate) || grepl('@',classfdate)){
         
           
          classfdate <- sub(".*:", "", classified_detail[2])  
        
           }
        
         
        post_date1 <- classfdate
        #post_date1 <- ymd(classfdate)
        # 
       
        
         #contact number
         contact_no <- classified_detail[5]
        
          
         if(grepl('Posting',contact_no)){
           
           
           contact_no <- sub(".*:", "", classified_detail[6])  
           
         }
       
         
        # other details
        classified_moredetailed <- html_nodes(classified_url,".post-body")
        classified_moredetailed <- gsub('<.*?>','|',classified_moredetailed)

        classified_moredetailed <-gsub("\n","",classified_moredetailed)
        classified_moredetailed <- unlist(str_split(classified_moredetailed,'[|]'))

  
        # make and model seperation
        make_model <- sub(".*:", "", classified_moredetailed[2])
        make_model <- trimws(make_model,'b')
  
  
        # vehicle mnufacturing year
  
        YOM <- sub(".*:", "", classified_moredetailed[3])
        YOM <- trimws(YOM,'b')

        
        
        # 
        # # vehicle transmission
        
        transmission <- sub(".*:", "", classified_moredetailed[4])
        transmission <- trimws(transmission,'b')
        
        
        if(grepl('manual',transmission) || grepl('automatic',transmission)){
          
          transmission <- transmission
        }else{
          
          transmission <- 'NA'
        }
        
      
        
        # #vehicle ODO Meter
        # 
        odo <- sub(".*:", "", classified_moredetailed[5])
        odo <- gsub("KM",'',odo)
        odo <- trimws(odo,'b')
        odo <- as.numeric(odo)
        # 
        # #combine all data
        # 
        
        vehicle <- c(link = i,post_date = classfdate,price = price,make_model = make_model,YOM=YOM,transmission=transmission,km_reading=odo,contact = contact_no)
       
      },
      error=function(cond) {
        
        # message(paste("URL does not seem to exist:", data2))
        # message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        vehicle <- c(link = i,post_date = NA,price = 0,make_model = NA,YOM=NA,transmission=NA,km_reading=0,contact = NA)
        
        
      },
      warning=function(cond) {
        # message(paste("URL caused a warning:", data2))
        # message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        #return(NA)
        vehicle <- c(link = i,post_date = NA,price = 0,make_model = NA,YOM=NA,transmission=NA,km_reading=0,contact = NA)
      },
      finally={
       
        
      }
    )    

  return(out)
 Sys.sleep(5)
  
   }
  
  

data1 <- pblapply(data, classified_data)

#convert to vector

mylist <- do.call('rbind',data1)

vehicle <- as.data.frame(mylist)


#generate CSV file

write.csv(mylist,file = 'expatriate_data2.csv')

# plot

library(tidyverse)
#install.packages("tidyverse")
ggplot(data = vehicle) + 
  geom_point(mapping = aes(x = YOM, y = price, color = transmission))