import sys
import getopt
import os
import twint
import time
import calendar
import pandas as pd

def get_tweets(hashtag, date_since, date_until, ts):
    
    t = twint.Config() #Inicializa
    t.Search = hashtag
    t.Since = date_since
    t.Until = date_until
    t.Retweets = True
    t.Pandas = True #Permitir integracao com pandas
    t.Store_csv = True #Escrever como csv
    t.Hide_output = True #Nao imprime no terminal
    t.Output = './Dados/tweets_' + hashtag + '_' + date_since + '_' + str(ts) +'.csv' #Arquivo saida
    twint.run.Search(t) #Executa a busca

    tweets_df = twint.storage.panda.Tweets_df

    return tweets_df


def get_retweets(hashtag, date_since, date_until, ts):
    
    rt = twint.Config() #Inicializa
    rt.Search = hashtag
    rt.Since = date_since
    rt.Until = date_until
    rt.Native_retweets = True
    rt.Pandas = True #Permitir integracao com pandas
    rt.Store_csv = True #Escrever como csv
    rt.Hide_output = True #Nao imprime no terminal
    rt.Output = './Dados/retweets_' + hashtag + '_' + date_since + '_' + str(ts) +'.csv' #Arquivo saida
    twint.run.Search(rt) #Executa a busca

    rt_df = twint.storage.panda.Tweets_df

    return rt_df

def get_handles(tweets, hashtag, ts):

    handles = tweets.username.unique()
    pd.DataFrame(handles).to_csv("./Dados/handles_" + hashtag + "_" + str(ts) + ".csv")

def main(argv):

    hashtag = ''
    date_since = ''
    date_until = ''
    all_tweets = pd.DataFrame()
    
    
    # Check parameters
    try:
        opts, args = getopt.getopt(argv, 'ht:s:u:',['term=','date_since=','date_until='])
        if len(opts) < 3: 
            raise getopt.GetoptError('')
    except getopt.GetoptError:
        print('Error or missing argument. \nusage: getData.py -t <term or hashtag> -s <data_since> -u <date_until>')
        print('Example: python getData.py -t #brasil 2022-01-05 00:00:00 2022-01-10 00:00:00')
        sys.exit(2)
   
    for opt, arg in opts:
        if opt == '-h':
            print('Usage: getData.py -t <term or hashtag> -s <data_since> -u <date_until>')
            print('Example: python getData.py -t brasil 2022-01-05 00:00:00 2022-01-10 00:00:00')
            sys.exit()
        if opt in ("-t", "--term"):
            hashtag = [arg]
        if opt in ("-s", "--date_since"):
            date_since = arg
        if opt in ("-u", "--date_until"):
            date_until = arg
         
    # Check if directory 'Dados' exists

    out_dir = os.path.exists('./Dados')
    if not out_dir:
        os.makedirs('./Dados')
        print('Created output directory: ./Dados')
       
    # Each output filename will receive a timestamp 
    ts = calendar.timegm(time.gmtime())
    
    # Collect data
    # TODO: ajustar para aceitar lista no parâmetro '-t' na linha de comando.
    # Por enquanto executando para um termo só
    for h in hashtag:
        print('----------------------------')
        print('Collecting ' + h + " since " + date_since) 
        
        print('Tweets')
        tweets = get_tweets(h, date_since, date_until, ts)

        print('Retweets')
        rt = get_retweets(h, date_since, date_until, ts)
        
        print('Join dataframes')
        tweets_rt = pd.concat([tweets, rt])
        
        tweets_rt.to_csv('./Dados/all_' + h + '_' + date_since + '_' + str(ts) + '.csv',
                         encoding="utf-8")
        
        all_tweets = pd.concat([tweets_rt, all_tweets])

        print('Collection completed: ' + h)
        #time.sleep(120)
        
    all_tweets.to_csv('./Dados/alltweets_' + date_since + '_' + str(ts) + '.csv',
                      encoding="utf-8")
    
    print('Selecting handles')
    get_handles(all_tweets, 'all', str(ts))
    print('Finished')

if __name__ == "__main__":
    main(sys.argv[1:])
