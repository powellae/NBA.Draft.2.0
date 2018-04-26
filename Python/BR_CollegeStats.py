import requests
from bs4 import BeautifulSoup as bs
import pandas as pd
import re
pages = 600
for page in range(0, pages):
    p = page*100
    url = "https://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2001&year_max=2018&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=pts&order_by_asc=&offset="+str(p)

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

    x = page/pages
    print(round(x,2))

df3.to_csv("br_college.csv")
