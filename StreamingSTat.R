library(tidyverse)
library(dplyr)
library(tidyr)

#loading in each dataset from their csv files
Netflix_path<-"/Users/jamesmcsweeney/Desktop/TV SHOW STAT PROJ/netflix_titles.csv"
Netflix<-read.csv(Netflix_path,header=TRUE)
Disney_path<-"/Users/jamesmcsweeney/Desktop/TV SHOW STAT PROJ/disney_plus_titles.csv"
Disney<-read.csv(Disney_path,header=TRUE)
Prime_path<-"/Users/jamesmcsweeney/Desktop/TV SHOW STAT PROJ/amazon_prime_titles.csv"
Prime<-read.csv(Prime_path,header=TRUE)
Hulu_path<-"/Users/jamesmcsweeney/Desktop/TV SHOW STAT PROJ/hulu_titles.csv"
Hulu<-read.csv(Hulu_path,header=TRUE)

#feature selection
#Selecting the columns to use. Did not include actor or director because of abundance of Missing values
#piped the select statements into an add column function to get the service column. This will be what we are predicting.
#In the target column the coding is hulu is 1,Disney is 2, Netflix is 3, Prime is 4
#additionally added a Child Friendliness column
Hulu<-Hulu%>%dplyr::select("show_id","type","title","country","date_added","release_year","rating","duration","listed_in","description")%>%add_column(Service=1)%>%mutate(ChildFriendly = if_else(`rating` == "TV-G"|`rating` == "G"|`rating` == "TV-Y"|`rating` == "TV-Y7", TRUE, FALSE ))
Disney<-Disney%>%dplyr::select("show_id","type","title","country","date_added","release_year","rating","duration","listed_in","description")%>%add_column(Service=2)%>%mutate(ChildFriendly = if_else(`rating` == "TV-G"|`rating` == "G"|`rating` == "Y"|`rating` == "Y7", TRUE, FALSE ))
Netflix<-Netflix%>%dplyr::select("show_id","type","title","country","date_added","release_year","rating","duration","listed_in","description")%>%add_column(Service=3)%>%mutate(ChildFriendly = if_else(`rating` == "TV-G"|`rating` == "G"|`rating` == "TV-Y"|`rating` == "TV-Y7"|rating=="	
ALL_AGES"|`rating`=="ALL"|`rating`=="7+", TRUE, FALSE ))
Prime<-Prime%>%dplyr::select("show_id","type","title","country","date_added","release_year","rating","duration","listed_in","description")%>%add_column(Service=4)%>%mutate(ChildFriendly = if_else(`rating` == "TV-G"|`rating` == "G"|`rating` == "TV-Y"|`rating` == "TV-Y7"|rating=="	
ALL_AGES"|`rating`=="ALL"|`rating`=="7+", TRUE, FALSE ))

separate(MegaDataset%>%separate_rows(listed_in),sep=", ")
#combinding each service into one large dataset
MegaDataset<-rbind(Hulu,Disney,Prime,Netflix)
#finding out the average release year for each service
Netflix_avg_Release<-sum(Netflix%>%dplyr::select(`release_year`))/8807
  Prime_avg_Release<-sum(Prime%>%dplyr::select(`release_year`))/9668
 Hulu_avg_Release<-sum(Hulu%>%dplyr::select(`release_year`))/3073
  Disney_avg_Release<-sum(Disney%>%dplyr::select(`release_year`))/1450
Average_Year_of_Release<-sum(MegaDataset%>%dplyr::select(`release_year`))/22998
#created a contentType column to spilt up content as movies or non movies
#Coding is 1 as a movie and 0 as a show
MegaDataset<-MegaDataset%>%mutate(ContentType=if_else(`type`=="Movie",1,0))
#Selected the final columns to be used
#the listedin column will continue to be split up into genres later on
MegaDataset<-MegaDataset%>%dplyr::select(title,date_added,release_year,rating,duration,listed_in,Service,ChildFriendly,ContentType)

#the genre spliting up process. Created a new column for each unqiue genre value
listedInTable <- MegaDataset %>% pull("listed_in") 
listedInTable
unlisting <- function(tbl){
  temp <- unlist(strsplit(tbl,","))
  newTemp <- vector(mode = 'character', length = length(temp))
  counter <- 1
  for (i in temp) {
    newTemp[counter] <- str_remove(i, "^ ")
    counter <- counter + 1
  }
  return(newTemp)
}
# Here i am unlisting all of the genres into a list
result <- unlisting(listedInTable)

# here i am finding all the unique genres
# there are 120 unique genres
uniqueGenre <- unique(result)

# addColumn simply adds the 120 genres to a tibble, 
# i only use it inside findGenre
addColumn <- function(tbl){
  for(genre in uniqueGenre){
    tbl <- tbl %>% mutate({{ genre }} := 0)
  }
  return(tbl)
}

# here is me putting 120 genre columns into the main table
updatedTotal <- addColumn(MegaDataset)

# this is the MONSTER function, it takes a tibble and
findGenre <- function(tbl){
  # add the 120 genre columns
  tbl <- addColumn(MegaDataset)
  # iterate through all rows
  for (i in 1:nrow(tbl)) {
    #print(i)
    # slice out a row and find its potential three genres
    row <- tbl %>% slice(n = i)
    rowGenre <- row %>% pull(listed_in) %>% unlisting()
    #print(rowGenre)
    #now we have the genres in rowGenre
    for (j in colnames(row)){
      #for each of the column names
      for (gen in rowGenre) {
        #we then iterate through each genre name
        if(gen == j){
          # and if a genre and a column name are the same
          # we change that column value to 1
          
          colIndex <- grep(j, colnames(row)) #get the column index
          #row[1, colIndex] <- 1 #replace the value with 1
          
          # and then have to replace the row back into the df
          
          #tbl <- tbl %>% right_join(row, by = j) #we update the row
          tbl[i, colIndex] <- 1
        }
      }
    }
    #then, finally we increment it all and start again!
  }
  return(tbl)
}

#running the function on our dataset
correctTotalTable <- findGenre(MegaDataset)

#Simlar genre names had to be combinded to have acurrate predictions
completeDATA<-correctTotalTable%>%mutate(Animate= if_else(`Adult Animation`==1| Cartoons==1| Animation==1|Anime==1|`Anime Series`==1|`Anime Features`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Cartoons,`Adult Animation`,Animation,Anime,`Anime Series`,`Anime Features`))
completeDATA<-completeDATA%>%mutate(sitcom=if_else(Sitcom==1|Buddy==1|`Soap Opera / Melodrama`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Sitcom,Buddy,`Soap Opera / Melodrama`))
completeDATA<-completeDATA%>%mutate(international= if_else(International==1|`International TV Shows`==1|`International Movies`==1|`Spanish-Language TV Shows`==1|`British TV Shows`==1|`Korean TV Shows`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(International,`International TV Shows`,`International Movies`,`Spanish-Language TV Shows`,`British TV Shows`,`Korean TV Shows`))
completeDATA<-completeDATA%>%mutate(history= if_else(Historical==1|Biographical==1|History==1|`Military and War`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Historical,Biographical,History,`Military and War`))
completeDATA<-completeDATA%>%mutate(romance= if_else(Romance==1|`Romantic TV Shows`==1|`Romantic Movies`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Romance,`Romantic TV Shows`,`Romantic Movies`))
completeDATA<-completeDATA%>%mutate(news= if_else(News==1|`Talk Show`==1|`Talk Show and Variety`==1|Disaster==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(News,`Talk Show`,`Talk Show and Variety`,Disaster))          
completeDATA<-completeDATA%>%mutate(health= if_else(`Health & Wellness`==1|`Faith and Spirituality`==1|`Faith & Spirituality`==1|Fitness==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(`Health & Wellness`,`Faith and Spirituality`,`Faith & Spirituality`,Fitness))   
completeDATA<-completeDATA%>%mutate(classic= if_else(Classics==1|`Classic Movies`==1|`Classic & Cult TV`==1|`Cult Movies`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Classics,`Classic Movies`,`Classic & Cult TV`,`Cult Movies`))  
completeDATA<-completeDATA%>%mutate(reality= if_else(Reality==1|`Reality TV`==1|Travel==1|Unscripted==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Reality,`Reality TV`,Travel,Unscripted))
completeDATA<-completeDATA%>%mutate(comedy= if_else(Comedy==1|Comedies==1|`Stand Up`==1|`Stand-Up Comedy`==1|`Sketch Comedy`==1|`Late Night`==1|`Stand-Up Comedy & Talk Shows`==1|`TV Comedies`==1|Parody==1|`Romantic Comedy`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Comedy,Comedies,`Stand Up`,`Stand-Up Comedy`,`Sketch Comedy`,`Late Night`,`Stand-Up Comedy & Talk Shows`,`TV Comedies`,Parody,`Romantic Comedy`))
completeDATA<-completeDATA%>%mutate(drama= if_else(Drama==1|Dramas==1|`TV Dramas`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Drama,Dramas,`TV Dramas`))
completeDATA<-completeDATA%>%mutate(crime= if_else(Crime==1|`Crime TV Shows`==1|`Spy/Espionage`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Crime,`Crime TV Shows`,`Spy/Espionage`))
completeDATA<-completeDATA%>%mutate(thriller= if_else(Thriller==1|Thrillers==1|Suspense==1|Mystery==1|`TV Mystery`==1|`TV Thrillers`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Thriller,Thrillers,Suspense,Mystery,`TV Mystery`,`TV Thrillers`))
completeDATA<-completeDATA%>%mutate(action= if_else(Action==1|`Action-Adventure`==1|`Action & Adventure`==1|Adventure==1|`TV Action & Adventure`==1|teen,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Action,`Action-Adventure`,`Action & Adventure`,Adventure,`TV Action & Adventure`))
completeDATA<-completeDATA%>%mutate(horror= if_else(Horror==1|`Horror Movies`==1|`TV Horror`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Horror,`Horror Movies`,`TV Horror`))
completeDATA<-completeDATA%>%mutate(Science= if_else(`Science Fiction`==1|`Science & Technology`==1|`Science & Nature TV`==1|`Sci-Fi & Fantasy`==1|Medical==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(`Science Fiction`,`Science & Technology`,`Science & Nature TV`,`Sci-Fi & Fantasy`))
completeDATA<-completeDATA%>%mutate(Under18= if_else(Kids==1|`Kids' TV`==1|Family==1|Teen==1|`Coming of Age`==1|`Young Adult Audience`==1|ChildFriendly==1|`Children & Family Movies`==1|`Teen TV Shows`==1|Teen==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Kids,`Kids' TV`,Family,Teen,`Coming of Age`,`Young Adult Audience`,ChildFriendly,`Children & Family Movies`,`Teen TV Shows`))
completeDATA<-completeDATA%>%mutate(Docs= if_else(Documentaries==1|Docuseries==1|Documentary==1|`Animals & Nature`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Documentaries,Docuseries,Documentary,`Animals & Nature`))
completeDATA<-completeDATA%>%mutate(lifestyle= if_else(Lifestyle==1|`Lifestyle & Culture`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Lifestyle,`Lifestyle & Culture`))
completeDATA<-completeDATA%>%mutate(ArtAndMusic= if_else(Music==1|Musical==1|`Music Videos and Concerts`==1|`Music & Musicals`==1|Arts==1|Arthouse==1|`Concert Film`==1|Anthology==1|Dance==1|`and Culture`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(Music=,Musical,`Music Videos and Concerts`,`Music & Musicals`,Arts,Arthouse,`Concert Film`,Anthology,Dance,`and Culture`))
completeDATA<-completeDATA%>%mutate(LGBTQ = if_else(`LGBTQ+`==1|`LGBTQ Movies`==1|LGBTQ==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(`LGBTQ+`,`LGBTQ Movies`,LGBTQ))
completeDATA<-completeDATA%>%mutate(GameShow = if_else(`Game Shows`==1|`Game Show / Competition`==1,1,0))
completeDATA<-dplyr::select(completeDATA,-c(`Game Shows`,`Game Show / Competition`))
completeDATA<-dplyr::select(completeDATA,-c(Movies,Variety,Series,`TV Show`))

#writing the dataset to a CSV file 
write_csv(completeDATA, "/Users/jamesmcsweeney/Desktop/TV SHOW STAT PROJ/streaming.csv")
#this is the end of the data cleaning process. See Classifcations.R for class downsampling and classifaction methods.




