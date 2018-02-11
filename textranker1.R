
# read in text file - just save a .txt file in your working directory
fileName = 'assignment.txt'


text = readChar(fileName, file.info(fileName)$size)

# find the length of the text, including spaces
length = nchar(text)

# split the text into a word list
text2 = strsplit(text, " ")

# count the number of words in the list
total_words = lengths(text2)

# count sentences
total_sentences = length(gregexpr('[[:alnum:] ][.!?]', text)[[1]])



find_syllable_count = function(text){
  
  # initialize syllable_count
  syllable_count = 0
  
  # convert to lower case
  text = tolower(text)
  
  # remove punctuation, numbers, return carriages, and double spaces
  text2 = gsub('[[:punct:]]','',text)
  text2 = gsub('[[:digit:]]','',text2)
  text2 = gsub('[\r\n]',' ',text2)
  text2 = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$",'',text2,perl=TRUE)
  
  # split text into words
  text3 = strsplit(text2, " ")
  text3 = as.data.frame(text3)
  
  vowels = c("a","e","i","o","u")
  
  # loops over each word
  for(i in 1:nrow(text3)){
    text4 = as.character(text3[i,])
    word = strsplit(text4,'')
    word = as.data.frame(word)
    
      if(word[nrow(word),] == 'e'){
        syllable_count = syllable_count - 1
      } 
    
      if(word[nrow(word),] == 'e'){
        if(word[nrow(word)-1,] == 'l'){
          if(!word[nrow(word)-2,] %in% vowels){
            syllable_count = syllable_count + 1 
          }
        }
      }
    
      # loops over each letter in each word
      for(j in 1:nrow(word)){
        if(word[j,] %in% vowels){
          if(!is.null(!word[j-1,] %in% vowels)){
            syllable_count = syllable_count + 1
          } 
        }

        else {
          syllable_count = syllable_count
        }
      }}
    return(syllable_count)
}

# call the function to count the syllables
total_syllables = find_syllable_count(text)





# calculate and print score
score = 206.835-1.015*(total_words/total_sentences)-84.6*(total_syllables/total_words)
score


# function to find the reading level of the text using the 
# result of the Flesch-Kincaid Reading Ease Formula
find_level = function(score){
  if(90 < score & score <= 100){
    level = "5th grade"}
  if(80 < score & score <= 90){
    level = "6th grade"}
  if(70 < score & score <= 80){
    level = "7th grade"}
  if(60 < score & score <= 70){
    level = "8th and 9th grade"}
  if(50 < score & score <= 60){
    level = "10th to 12th grade"}
  if(30 < score & score <= 50){
    level = "College"}
  if(0 < score & score <= 30){
    level = "College graduate"}
return(level)
}

# call function to find the reading level
find_level(score)

