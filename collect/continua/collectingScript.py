import sys
import os
import twint
import getopt
import psycopg2
import pandas as pd
import psycopg2.extras as extras
from datetime import datetime
from dotenv import load_dotenv

def conn_db():

    load_dotenv()
    
    DB_NAME = os.getenv('dbname')
    DB_USER = os.getenv('user')
    DB_HOST = os.getenv('host')
    DB_PASSWD = os.getenv('password')
    
    #print("dbname="+ DB_NAME + " user=" + DB_USER + " host=" + DB_HOST + " password=" + DB_PASSWD)
   
    conn = psycopg2.connect("dbname="+ DB_NAME + " user=" + DB_USER + " host=" + DB_HOST + " password=" + DB_PASSWD)
    
    return conn

def create_db(conn):
    
    cur = conn.cursor()

    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS tweets (
            id VARCHAR(100),
            scrapping_date VARCHAR(15),
            scrapping_term VARCHAR(50),
            conversation_id VARCHAR(100),
            date_time VARCHAR(20),
            timezone INTEGER,
            user_id VARCHAR(100),
            username VARCHAR(15),
            name VARCHAR(50),
            tweet VARCHAR(500),
            language VARCHAR(10),
            replies_count INTEGER,
            retweets_count INTEGER,
            likes_count INTEGER,
            hashtags VARCHAR(400),
            link VARCHAR(100),
            quote_url VARCHAR(100),
            video INTEGER,
            reply_to VARCHAR(10000),
            thumbnail VARCHAR(150)
        );
    """
    )

    conn.commit()

    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS retweets (
            id VARCHAR(100),
            scrapping_date VARCHAR(15),
            scrapping_term VARCHAR(50),
            conversation_id VARCHAR(100),
            date_time VARCHAR(20),
            timezone INTEGER,
            user_id VARCHAR(100),
            username VARCHAR(15),
            name VARCHAR(50),
            tweet VARCHAR(500),
            language VARCHAR(10),
            replies_count INTEGER,
            retweets_count INTEGER,
            likes_count INTEGER,
            hashtags VARCHAR(400),
            link VARCHAR(100),
            video INTEGER,
            user_rt_id VARCHAR(100),
            user_rt VARCHAR(500),
            retweet_date VARCHAR(25),
            retweet_id VARCHAR(100),
            thumbnail VARCHAR(150)
        );
    """
    )

    conn.commit()

    cur.close()
    #conn.close()

def insert_db_tweets(conn, dataframe, date, term):
    """
    Using psycopg2.extras.execute_values() to insert the dataframe
    """

    # Create a list of tupples from the dataframe values
    #tuples = [tuple(x) for x in dataframe.to_numpy()]
    
    
    tuples = []
    for x in dataframe.to_numpy():
        tuples.append((str(x[0]), str(date), str(term), str(x[1]), str(x[3]), 
                        str(x[4]), str(x[11]), str(x[12]), str(x[13]),
                        str(x[6]), str(x[7]), str(x[23]), str(x[24]), str(x[22]), 
                        str(x[8]), str(x[16]), str(x[25]), str(x[19]), str(x[33]), str(x[20])))
    
    # SQL quert to execute
    query = """
        INSERT into tweets(id, scrapping_date, scrapping_term, conversation_id, 
            date_time, timezone, user_id, username, name, tweet, language, replies_count,
            retweets_count, likes_count, hashtags, link, quote_url, video, reply_to, thumbnail)
            VALUES %s;
        """

    cursor = conn.cursor()
    try:
        extras.execute_values(cursor, query, tuples)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
        conn.rollback()
        cursor.close()
        return 1
    print("insert_db_tweets() done")
    cursor.close()

def insert_db_retweets(conn, dataframe, date, term):
    """
    Using psycopg2.extras.execute_values() to insert the dataframe
    """

    # Create a list of tupples from the dataframe values
    #tuples = [tuple(x) for x in dataframe.to_numpy()]

    tuples = []
    for x in dataframe.to_numpy():
        tuples.append((str(x[0]), str(date), str(term), str(x[1]), str(x[3]), 
                        str(x[4]), str(x[11]), str(x[12]), str(x[13]),
                        str(x[6]), str(x[7]), str(x[23]), str(x[24]), str(x[22]), 
                        str(x[8]), str(x[16]), str(x[19]), str(x[30]),
                        str(x[31]), str(x[34]), str(x[32]), str(x[20])))
        
    # SQL quert to execute
    query = """
        INSERT into retweets(id, scrapping_date, scrapping_term, conversation_id, date_time, 
            timezone, user_id, username, name, tweet, language, replies_count,
            retweets_count, likes_count, hashtags, link, video, user_rt_id,
            user_rt, retweet_date, retweet_id, thumbnail)
            VALUES %s;
        """

    cursor = conn.cursor()
    try:
        extras.execute_values(cursor, query, tuples)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
        conn.rollback()
        cursor.close()
        return 1
    print("insert_db_retweets() done")
    cursor.close()

def get_tweets(hashtag, date_since, date_until):
    
    t = twint.Config() # Inicializa
    t.Search = hashtag
    t.Since = date_since
    t.Until = date_until
    t.Retweets = True
    t.Pandas = True # Permitir integracao com pandas
    t.Hide_output = True # Nao imprime no terminal
    twint.run.Search(t) # Executa a busca

    tweets_df = twint.storage.panda.Tweets_df

    return tweets_df


def get_retweets(hashtag, date_since, date_until):

    rt = twint.Config() #Inicializa
    rt.Search = hashtag
    rt.Since = date_since
    rt.Until = date_until
    rt.Native_retweets = True
    rt.Pandas = True #Permitir integracao com pandas
    rt.Hide_output = True #Nao imprime no terminal
    twint.run.Search(rt) #Executa a busca

    rt_df = twint.storage.panda.Tweets_df

    return rt_df

def main(argv):

    hashtag = ''
    date_since = ''
    date_until = ''
    
    # Check parameters
    try:
        opts, args = getopt.getopt(argv, 'ht:s:u:',['term=','date_since=','date_until='])
        if len(opts) < 3: 
            raise getopt.GetoptError('')
    except getopt.GetoptError:
        print('Error or missing argument. \nusage: getData.py -t <term or hashtag> -s <data_since> -u <date_until>')
        print('Example: python getData.py -t #brasil -s 2022-01-05 -u 2022-01-10')
        sys.exit(2)
   
    for opt, arg in opts:
        if opt == '-h':
            print('Usage: getData.py -t <term or hashtag> -s <data_since> -u <date_until>')
            print('Example: python getData.py -t brasil -s 2022-01-05 -u 2022-01-10')
            sys.exit()
        if opt in ("-t", "--term"):
            hashtag = [arg]
        if opt in ("-s", "--date_since"):
            date_since = arg
        if opt in ("-u", "--date_until"):
            date_until = arg
    
    # Creating database
    conn = conn_db()
    
    create_db(conn)
    
    # Each search made will receive a date
    date = datetime.today().strftime('%d-%m-%Y')
    
    # Collect data
    # TODO: ajustar para aceitar lista no parâmetro '-t' na linha de comando.
    # Por enquanto executando para um termo só
    for term in hashtag:
        print('----------------------------')
        print('Collecting ' + term + " since " + date_since) 
        
        print('Tweets')
        # Scrapping tweets
        tweets = get_tweets(term, date_since, date_until)
        # Inserting in the database
        insert_db_tweets(conn, tweets, date, term)

        print('Retweets')
        # Scrapping retweets
        rt = get_retweets(term, date_since, date_until)
        # Inserting in the database
        insert_db_retweets(conn, rt, date, term)

        print('Collection completed: ' + term)
    
    conn.close()

if __name__ == "__main__":
    main(sys.argv[1:])
