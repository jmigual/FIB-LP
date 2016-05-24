#!/usr/bin/python
# -*- coding: utf-8 -*-

import csv
import sys
import ast
from math import *

class Location:    
  def __init__(self, lat = 0, lon = 0):
    self.lat = radians(lat)
    self.lon = radians(lon)
    
  def distanceTo(self, loc):
    R = 6.371e6 # metres
    phiInc = loc.lat - self.lat
    lambInc = loc.lon - self.lon
    
    a = sin(phiInc/2)**2 + cos(self.lat) * cos(loc.lat) + sin(lambInc/2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    return R * c
  
  def __str__(self):
    latD = degrees(self.lat)
    lonD = degrees(self.lon)
    
    ret = str(latD) + ("N" if latD > 0  else "S") + " "
    ret += str(lonD) + ("E" if lonD > 0 else "W")
    return ret

class Restaurant:
  def __init__(self, info):
    self.name       = info[0]
    self.address    = info[1]
    self.district   = info[2]
    self.postal     = info[3]
    self.town       = info[4]
    self.country    = info[5]
    self.tel        = [info[6], info[7]]
    self.web        = info[8]
    self.email      = info[9]
    self.location   = Location(float(info[10]), float(info[11]))
    self.extra      = []
    
    if len(info) >= 13:
      self.extra.append(info[12])
    if len(info) >= 14:
      self.extra.append(info[13])
    if len(info) >= 15:
      self.extra.append(info[14])
    
  def __str__(self):
    return self.name + " Location: " + str(self.location)

class Query:
  def __init__(self, queryArr):
    self.arr = queryArr
    
  def satisfies(self, text):
    return Query.__satisfies(text, self.arr)
    
  @staticmethod
  def __satisfies(words, query):
    if type(query) is str:
      return words.find(query) != -1
    elif type(query) is tuple:
      return all(map(Query.__satisfies, [words]*len(query), query))
    elif type(query) is list:
      return any(map(Query.__satisfies, [words]*len(query), query))
    return False

def main():
  if len(sys.argv) != 2:
    print "Usage: cerca.py <List of parameters>"
    return
  
  # Convert string to tuple and array
  queryArr = ast.literal_eval(sys.argv[1])
  query = Query(queryArr)
  
  # Read restaurants info
  with open('restaurants.csv','r') as csvfile:
    reader = csv.reader(csvfile, delimiter='\t')
    
    first = True
    for row in reader:      
      # Ignore first row
      if first:
        first = False
        continue
      
      rest = Restaurant(row)
      if query.satisfies(rest.name):
        print rest.name
        

if __name__ == "__main__":
  main()
  
  