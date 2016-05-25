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

  def getLatitude(self):
    return degrees(self.lat)

  def getLongitude(self):
    return degrees(self.lon)

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
    self.bicings    = []

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


def searchRestaurants(query):
  res = []

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
        res.append(rest)
  return res

def getBicings():
  return

def wColumn(data):
  return "<td>" + data + "</td>"

def writeHTMLrestaurants(res):
  # First of all write the headers of the html page
  with open('restaurants.html', 'w') as file:
    file.write("""
      <!DOCTYPE html>
      <head>
        <!-- Latest compiled and minified CSS -->
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">
        <meta charset="UTF-8">
        <title>Restaurants amb bicis</title>
        <meta name="author" content="joan.marce.igual@est.fib.upc.edu" >
        <style>
          table {
            table-layout: auto;
            white-space: nowrap;
          }
        </style>
      </head>
      <body>
        <table class="table table-striped">
          <thead>
            <tr>
              <th>#</th>
              <th>NOM</th>
              <th>ADREÇA</th>
              <th>C.P.</th>
              <th>POBLACIÓ</th>
              <th>PAÍS</th>
              <th>Telèfon 1</th>
              <th>Telèfon 2</th>
              <th>WEB</th>
              <th>e-mail</th>
              <th>LATITUD</th>
              <th>LONGITUD</th>
              <th>INFO EXTRA 1</th>
              <th>INFO EXTRA 2</th>
              <th>INFO EXTRA 3</th>
            </tr>
          </thead>
          <tbody> """)

    td = "<td>"
    tdc = "<tdc>"
    i = 1
    l = len(res)
    for rest in res:
      print i, "of", l
      file.write("<tr>" + "<th scope='row'>" + str(i) + "</th>" + 
        wColumn(rest.name) +
        wColumn(rest.address) + 
        wColumn(rest.postal) +
        wColumn(rest.town) + 
        wColumn(rest.country) +
        wColumn(rest.tel[0]) +
        wColumn(rest.tel[1]) +
        wColumn("<a href='" + rest.web + "'>" + rest.web + "</a>") + 
        wColumn("<a href='mailto:" + rest.email + "'>" + rest.email + "</a>") +
        wColumn(str(rest.location.getLatitude())) + 
        wColumn(str(rest.location.getLongitude())) +
        wColumn(rest.extra[0] if len(rest.extra) > 0 else "") +
        wColumn(rest.extra[1] if len(rest.extra) > 1 else "") +
        wColumn(rest.extra[2] if len(rest.extra) > 2 else "") +
        "</tr>")
      i += 1

    file.write("</tbody></table></body>")


################
####  MAIN  ####
################

def main():
  if len(sys.argv) != 2:
    print "Usage: cerca.py <List of parameters>"
    return
  
  # Convert string to tuple and array
  queryArr = ast.literal_eval(sys.argv[1])
  
  res = searchRestaurants(Query(queryArr))
  
  if len(res) <= 0:
    print "No restaurants found"
    return

  print len(res), "restaurants found:"
  for rest in res:
    print rest.name

  print "\nWriting data to restaurants.html, please wait"
  writeHTMLrestaurants(res)

if __name__ == "__main__":
  main()
  
  