import requests
from bs4 import BeautifulSoup as bs
import pandas as pd
import re

for page in range(0, 17):
    year = 2000 + page
    url = "https://www.basketball-reference.com/awards/recruit_rankings_"+str(year)+".html"

    response = requests.get(url)
    html = response.content
    soup = bs(html, 'lxml')

    table = soup.findAll('tr')[1]

    df_headers = []
    for item in table.findAll('th'):
        df_headers.append(item.text.strip())

    df_headers.remove('Rk')

    df_dict = {}
    df_idx_ref = {}
    idx = 0

    for name in df_headers:
        df_dict[name] = []
        df_idx_ref[idx] = name
        idx += 1

    rows = soup.findAll('tr')[2:]

    for row in rows:
        data = row.findAll('td')[:27]
        idx = 0
        for d in data:
            df_dict[df_idx_ref[idx]].append(d.text.strip())
            idx += 1

    # Print first five entries of df_dict
    # for key in df_dict:
    #    print('{}: {}\n'.format(key, df_dict[key][0:5]))

    df = pd.DataFrame(df_dict, columns=df_dict.keys())


    df2 = pd.DataFrame(columns=df_dict.keys())

    if page == 0:
        frames = [df, df2]
        df3 = pd.concat(frames)
    if page > 0:
        frames = [df, df3]
        df3 = pd.concat(frames)


df3.to_csv("br_rsci.csv")
