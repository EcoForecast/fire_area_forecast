# This script read csv of GEFS temperature.

dir = list.dirs('/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/GEFS')[-1]
output_csv1 = '/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/GEFS/summary_data.csv'
output_csv2 = '/usr3/graduate/tmccabe/mccabete/Fire_forecast_509/data/GEFS/summary_data_8days.csv'
len = length(dir)
print(len)
data_out = matrix(NA, nrow=len-1, ncol=3)
data_out2 = matrix(NA, nrow=len-1, ncol=3)
i=0
for (f in dir){
  i=i+1
  #if (i==len){  # right now the 0328 data is missing
   # break
  #}
  #print(f)
  date = basename(f)
  #csv_path = list.files(dir=f, pattern='*.csv')
  csv_path = Sys.glob(file.path(f, '*current_meteorology.csv'))
  #print(csv_path)
  data = read.csv(file=csv_path, header=TRUE, sep=';', stringsAsFactors=FALSE)
  data_new = gsub(',', '.', data)
  temp = data_new[2]
  data_out[i, 1] = date
  data_out[i, 2] = temp
  precip = data_new[3]
  data_out[i, 3] = precip
  #data_out[i, 1] <- date 
  #data_out[i, 2] <- temp
  #print(csv_path)
  #print(data_new)
  #print(temp)
  #print(date)
  #print(data_out)
  #print(date)
 
 # timeDiff <- lubridate::as_date(as.character(date)) - lubridate::as_date('20190306') # Convertget difference in days
  timeDiff <- as.Date(date,format='%Y%m%d') - as.Date('20190306',format='%Y%m%d') 
   timeDiff <- as.numeric(timeDiff) 
  if(timeDiff%%8==0){
    data_out2[i,1] = data_out[i,1] 
    data_out2[i,2] = data_out[i,2] 
    data_out2[i,3] = data_out[i,3] 
  }
}  

write.table(data_out, file=output_csv1, col.names=FALSE, sep=',')
write.table(data_out2, file=output_csv2, col.names=FALSE, sep=',')
