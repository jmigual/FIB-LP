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

  def addName(self,name):
    self.name = name
    
  def addAddress(self, address):
    self.address = address
    
  def addLocation(self, location):
    self.location = location
    
  def addTel(self, telf):
      if "+" in telf:
        self.telfs.append(telf)
        
  def addWeb(self, web):
    self.web = web
    
  def addEmail(self, email):
    self.email = email
    
class Address:
  
  
  def __init__(self):
    self.streetName = ""
    self.streetNumber = ""
    self.district = ""
    self.postalCode = ""
    self.locality = ""
    self.region = ""
    self.country = ""
  
  def addStreetName(self, name):
    self.streetName = name
  
  def addStreetNumber(self, number):
    self.streetNumber = number
    
  def addDistrict(self, district):
    self.district = district
  
  def addPostalCode(self, postalCode):
    self.postalCode = postalCode
    
  def addLocality(self, locality):
    self.locality = locality
    
  def addRegion(self, region):
    self.region = region
    
  def addCountry(self, country):
    self.country = country
    
class Location:
  
  latitude = 0
  longitude = 0
  
  def addLatitude(self, latitude):
    self.latitude = latitude
    
  def addLongitude(self, longitude):
    self.longitude = longitude
    


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
      self.crest.addAddress(Address())
    elif tag == 'v:location':
      self.crest.addLocation(Location())
    elif tag == 'v:email':
      self.ctop = tag
    elif tag == 'v:url':
      for t in attrs:
        if len(t) >= 2 and t[0] == 'rdf:resource':
          self.crest.addWeb(t[1])
    elif tag == 'rdf:description' and self.ctop == 'v:email':
      for t in attrs:
        if len(t) >= 2 and t[0] == 'rdf:about':
          self.crest.addEmail(t[1].replace("mailto:", ""))

  def handle_endtag(self, tag):
    self.ctag = ""
    if tag == 'v:vcard':
      allrest.append(self.crest)
    if tag == self.ctop:
      self.ctop = ""

  def handle_data(self, data):
    if self.ctag == 'v:fn':
      self.crest.addName(data)
      
    elif self.ctag == 'xv:streetname':
      self.crest.address.addStreetName(data)
    elif self.ctag == 'xv:streetnumber':
      self.crest.address.addStreetNumber(data)
    elif self.ctag == 'v:region':
      self.crest.address.addRegion(data)
    elif self.ctag == 'xv:district':
      self.crest.address.addDistrict(data)
    elif self.ctag == 'v:postal-code':
      self.crest.address.addPostalCode(data)
    elif self.ctag == 'v:locality':
      self.crest.address.addLocality(data)
    elif self.ctag == 'v:country-name':
      self.crest.address.addCountry(data)
      
    elif self.ctag == 'v:latitude':
      self.crest.location.addLatitude(float(data))
    elif self.ctag == 'v:longitude':
      self.crest.location.addLongitude(float(data))
      
    elif self.ctag == 'rdf:value':
      self.crest.addTel(data)
    elif self.ctag == 'v:url':
      self.crest.addWeb(data)
      

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

