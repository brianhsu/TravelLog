package org.travellog.installer

import java.io.File
import java.io.PrintWriter
import java.io.FileWriter

import net.liftweb.util.StringHelpers


object Insteller
{

  def buildConfigFile(configFile: File)
  {
    println(
      """
      | #################################################################
      | # Setup PostgresSQL Database
      | #
      | # Please follow the instruction to setup your database connection.
      | #
      """.stripMargin
    ) 

    val dbHost = ConsoleReader.getLine("Please enter your DB host [localhost]:", "localhost")
    val dbPort = ConsoleReader.getLine("Please enter your DB port [5432]:", "5432")
    val dbName = ConsoleReader.getLine("Please enter your DB database name:")
    val dbUsername = ConsoleReader.getLine("Please enter your DB username:")
    val dbPassword = ConsoleReader.getLine("Please enter your DB database password:")
    val host = ConsoleReader.getLine("Please enter your website URL [http://localhost:8081]:", "http://localhost:8081")
    val imgUrKey = ConsoleReader.getLine("Please enter your ImgUr API key for get images:")
    val imgUrSecret = ConsoleReader.getLine("Please enter your ImgUr API secret for get images:")
    val imgUrLoginKey = ConsoleReader.getLine("Please enter your ImgUr API key for log in:")
    val imgUrLoginSecret = ConsoleReader.getLine("Please enter your ImgUr API secret for log in:")

    val flickrKey = ConsoleReader.getLine("Please enter your Flickr API key:")
    val flickrSecret = ConsoleReader.getLine("Please enter your Flickr API secret:")

    val picasaWebKey = ConsoleReader.getLine("Please enter your PicasaWeb API key:")
    val picasaWebSecret = ConsoleReader.getLine("Please enter your PicasaWeb API secret:")

    val configFileContent = raw"""
      |# Database Setting
      |DB_HOSTNAME = ${dbHost}
      |DB_USERNAME = ${dbUsername}
      |DB_PASS = ${dbPassword}
      |DB_NAME = ${dbName}
      |
      |# URL of your site
      |HOST = ${host}
      |
      |# ImgUr API Key / Secret for get images from ImgUr
      |IMGUR_APIKEY = ${imgUrKey}
      |IMGUR_SECRET = ${imgUrSecret}
      |
      |# ImgUr API Key / Secret for log in using ImgUr
      |IMGUR_LOGIN_APIKEY = ${imgUrLoginKey}
      |IMGUR_LOGIN_SECRET = ${imgUrLoginSecret}
      |
      |# Flickr API Key / Secret
      |FLICKR_APIKEY = ${flickrKey}
      |FLICKR_SECRET = ${flickrSecret}
      |
      |# PicasaWeb API Key / Secret
      |PICASAWEB_KEY = ${picasaWebKey}
      |PICASAWEB_SECRET = ${picasaWebSecret}
      |
    """.stripMargin

    val printWriter = new PrintWriter(configFile)
    printWriter.println(configFileContent)
    printWriter.close()

  }

  def main(args: Array[String])
  {
    val runMode = ConsoleReader.getChoice("Run mode", List("development", "production", "test"))

    val configFile = runMode match {
      case "development" => new File("src/main/resources/default.props")
      case "production" => new File("src/main/resources/production.default.props")
      case "test" => new File("src/main/resources/test.default.props")
    }

    configFile.exists match {
      case true  => 
        println("===============================")
        println("[note] You already have src/main/resources/default.props")
        println("[note] please edit it directly if you want to change your configuration.")
        println("===============================")
      case false => 
        buildConfigFile(configFile)
    }
  }
}
