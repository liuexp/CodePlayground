#!/usr/bin/env python
# coding=utf-8
#import cgi
print 'Content-Type: text/html'
print 
print 
print '<html><title>这是中文</title>'
print '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />'
#http://math.sjtu.edu.cn/faculty/Homepage.do?op=xsbaogao&pageno=1
import urllib2
#import simplejson as json
from django.utils import simplejson as json
from datetime import date
import time
import re


rawdata=urllib2.urlopen('http://math.sjtu.edu.cn/faculty/Homepage.do?op=xsbaogao&pageno=1')
data=json.load(rawdata)
for line in data:
	#line=unicode(line,'UTF-8')
	#print line
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
			print line['cname'].encode('UTF-8')
			print content['address'].encode('UTF-8')
			print "<p><a href='http://math.sjtu.edu.cn/Information/Information.html?id="+str(line['id'])+"'>"
			print line['title'].encode('UTF-8')
			print "</a></p>"
			print "<br/>"

from HTMLParser import HTMLParser
class SJTU_lecture_Parser(HTMLParser):
	def __init__(self):
		HTMLParser.__init__(self)
		self.intag = ''
	def handle_data(self,data):
		if self.intag == 'p' or self.intag == 'P':
			print data
		
	def handle_starttag(self, tag, attrs):
		self.intag=tag
	def handle_endtag(self, tag):
		self.intag=''

def getdate(line2):
	DATE=date.today()
	if "时间：" in line2 or "时间:" in line2:
					dstr=re.search(repr(DATE.year),line2)
					if dstr == None:
						dstr=re.search("([0-1]?[0-9])[ ]?月[ ]?([0-3]?[0-9])[ ]?日",line2)
						if dstr== None or (int(dstr.group(1)) not in range(1,13)) or (int(dstr.group(2)) not in range(1,32)):
							dstr=re.search("([0-1]?[0-9])-([0-3]?[0-9])",line2)
						if dstr== None or (int(dstr.group(1)) not in range(1,13)) or (int(dstr.group(2)) not in range(1,32)):
							dstr=re.search("([0-1]?[0-9])\.([0-3]?[0-9])",line2)
						if dstr != None:
							DATE=date(DATE.year,int(dstr.group(1)),int(dstr.group(2)))
						#else :
							#print line2
						
					else :
						dstr=re.search("年[ ]?([0-1]?[0-9])[ ]?月[ ]?([0-3]?[0-9])[ ]?日",line2)
						if dstr== None or (int(dstr.group(1)) not in range(1,13)) or (int(dstr.group(2)) not in range(1,32)):
							dstr=re.search("-([0-1]?[0-9])-([0-3]?[0-9])",line2)
						
						if dstr== None or (int(dstr.group(1)) not in range(1,13)) or (int(dstr.group(2)) not in range(1,32)):
							dstr=re.search("\.([0-1]?[0-9])\.([0-3]?[0-9])",line2)
						if dstr != None:
							DATE=date(DATE.year,int(dstr.group(1)),int(dstr.group(2)))
						#else : print line2
	return DATE

for line in urllib2.urlopen('http://www.sjtu.edu.cn/xiaoli/xlactivitylist.jsp?wbtreeid=1251'):
	#line=unicode(line,'UTF-8')
	if 'xlactivitycontent' in line and '1251' in line:  #re.search('xlactivitycontent',line) != None:  #
		#print line
		DATE=date.today()
		surl=re.search('wbtreeid=1251&[^"]+',line)
		if surl != None:
			url=surl.group(0)
			url="http://www.sjtu.edu.cn/xiaoli/xlactivitycontent.jsp?urltype=news.NewsContentUrl&"+url
			#print url
			#I don't know how to retrieve the data at this moment in a very nice way.
			if DATE == date.today():
				for line2 in urllib2.urlopen(url):
					DATE=getdate(line2)
					if DATE != date.today(): break
			if DATE >= date.today():
				for line2 in urllib2.urlopen(url):
					if "时间：" in line2 or "报告人：" in line2 or "地点：" in line2 or "时间:" in line2 or "地点:" in line2 or "报 告 人:" in line2:
						print "<p>"
						lpar2=SJTU_lecture_Parser()
						lpar2.feed(line2)
						print "</p>"

		stitle=re.search('title="(.+)" style',line)
		if stitle != None :
			title=stitle.group(1)
			#title=title[7:-3]
			if DATE >= date.today():
				print "<p><a href='"+url+"'>"
				print title
				print "</a></p>"
				print "\n<br/>"
		else :print line

print "</html>"
