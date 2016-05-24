#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import csv

from HTMLParser import HTMLParser

allrest=[]

class Restaurant: 
  
  def __init__(self):
    self.telfs = []
    self.web = ""
    self.email = ""
    
class Address:
  
  
  def __init__(self):
    self.streetName = ""
    self.streetNumber = ""
    self.district = ""
    self.postalCode = ""
    self.locality = ""
    self.region = ""
    self.country = ""
    
class Location:
  
  def __init__(self):
    self.latitude = 0
    self.longitude = 0
  
  
# creem una subclasse i sobreescribim el metodes del han
class MHTMLParser(HTMLParser):

  crest = Restaurant()
  ctag = ""
  ctop = ""

  def handle_starttag(self, tag, attrs):
    self.ctag = tag
    
    if tag == 'v:vcard':
      self.crest = Restaurant()
    elif tag == 'v:address':
      self.crest.address = Address()
    elif tag == 'v:location':
      self.crest.location = Location()
    elif tag == 'v:email':
      self.ctop = tag
    elif tag == 'v:url':
      for t in attrs:
        if len(t) >= 2 and t[0] == 'rdf:resource':
          self.crest.web =t[1]
    elif tag == 'rdf:description' and self.ctop == 'v:email':
      for t in attrs:
        if len(t) >= 2 and t[0] == 'rdf:about':
          self.crest.email = t[1].replace("mailto:", "")

  def handle_endtag(self, tag):
    self.ctag = ""
    if tag == 'v:vcard':
      allrest.append(self.crest)
    if tag == self.ctop:
      self.ctop = ""

  def handle_data(self, data):
    # Remove spaces
    data = data.strip()
    if self.ctag == 'v:fn':
      self.crest.name = data
      
    elif self.ctag == 'xv:streetname':
      self.crest.address.streetName = data
    elif self.ctag == 'xv:streetnumber':
      self.crest.address.streetNumber = data
    elif self.ctag == 'v:region':
      self.crest.address.region = data
    elif self.ctag == 'xv:district':
      self.crest.address.district  = data
    elif self.ctag == 'v:postal-code':
      self.crest.address.postalCode = data
    elif self.ctag == 'v:locality':
      self.crest.address.locality = data
    elif self.ctag == 'v:country-name':
      self.crest.address.country = data
      
    elif self.ctag == 'v:latitude':
      self.crest.location.latitude = float(data)
    elif self.ctag == 'v:longitude':
      self.crest.location.longitude = float(data)
      
    elif self.ctag == 'rdf:value' and "+" in data:
      self.crest.telfs.append(data)
    elif self.ctag == 'v:url':
      self.crest.web = data
      

f = open('restaurants.rdf', 'rb') # obre l'arxiu
rdfSource = f.read()                            
f.close()

print "Parsing elements wait a moment please"

parser = MHTMLParser()
parser.feed(rdfSource)

csvfile = open('restaurants.csv', 'w')
writer = csv.writer(csvfile, delimiter='\t')
writer.writerow(["NOM", "ADREÇA", "DISTRICTE", "C. P.", "POBLACIÓ", "PAÍS", "Telèfon 1", "Telèfon 2", "WEB", "E-MAIL", "LATITUD", "LONGITUD", "INFO EXTRA 1", "INFO EXTRA 2", "INFO EXTRA 3" ])
print len(allrest), "elements found"

j = 1
for r in allrest:
  print "Writing element", j, "of", len(allrest)
  
  res = []
  res.append(r.name)
  res.append(r.address.streetName + " " + r.address.streetNumber)
  res.append(r.address.district)
  res.append(r.address.postalCode)
  res.append(r.address.locality)
  res.append(r.address.country)
  
  i = 0
  while i < len(r.telfs) and i < 2:
    res.append(r.telfs[i])
    i += 1
    
  while i < 2:
    res.append("")
    i += 1
  
  res.append(r.web)
  res.append(r.email)
  res.append(r.location.latitude)
  res.append(r.location.longitude)
  
  if len(r.telfs) > 2:
    i = 2
    while i < len(r.telfs):
      res.append(r.telfs[i])
      i += 1
  writer.writerow(res)
  j += 1
  
print "Job done enjoy it"

