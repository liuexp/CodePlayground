#http://math.sjtu.edu.cn/faculty/Homepage.do?op=xsbaogao&pageno=1
import urllib2
import simplejson as json
from datetime import date
import time
import re
rawdata=urllib2.urlopen('http://math.sjtu.edu.cn/faculty/Homepage.do?op=xsbaogao&pageno=1')
data=json.load(rawdata)
for line in data:
    dstr=re.search("(\d*)-(\d*)-(\d*)",line['cdate'])
    if dstr!=None:
        DATE=date(int(dstr.group(1)),int(dstr.group(2)),int(dstr.group(3)))
        if DATE >= date.today():
            #print line['cdate']
            #next came further exploration
            url='http://math.sjtu.edu.cn/faculty/Homepage.do?op=xsbaogao&id='+str(line['id'])
            rawcontent=urllib2.urlopen(url)
            content=json.load(rawcontent)
            print content['cdate']+content['dttime']
            
            print line['cname']
            print content['address']
            print line['title']
            print
