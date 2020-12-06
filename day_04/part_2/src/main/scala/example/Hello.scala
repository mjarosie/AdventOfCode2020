package example

import scala.io.Source
import scala.util.matching.Regex

sealed trait Document

case class Passport(byr: String, iyr: String, eyr: String, hgt: String, hcl: String, ecl: String, pid: String, cid: String) extends Document

object Passport {
  def mapContainsAllProperties(props: Map[String, String]) : Boolean = {
    if (props.contains("byr") && props.contains("iyr") && props.contains("eyr") && 
        props.contains("hgt") && props.contains("hcl") && props.contains("ecl") && 
        props.contains("pid") && props.contains("cid")) {
        true
    }
    else {
      false
    }
  }
}

case class NorthPoleCredentials (byr: String, iyr: String, eyr: String, hgt: String, hcl: String, ecl: String, pid: String) extends Document

object NorthPoleCredentials {
  def mapContainsAllProperties(props: Map[String, String]) : Boolean = {
    if (props.contains("byr") && props.contains("iyr") && props.contains("eyr") && 
        props.contains("hgt") && props.contains("hcl") && props.contains("ecl") && 
        props.contains("pid")) {
        true
    }
    else {
      false
    }
  }
}

case class Invalid() extends Document

object Invalid {
  def mapContainsAllProperties(props: Map[String, String]) : Boolean = {
    true
  }
}

object Hello extends App {
  val filename = "input.txt"
  val inputLines = Source.fromFile(filename).getLines().toList

  val (lastItemProperties, items) = inputLines.foldLeft(Map[String, String](), List[Map[String, String]]()) ((acc: (Map[String, String], List[Map[String, String]]), line: String) => {
    val currentItemPropertiesAccumulator = acc._1
    val itemsSoFar = acc._2
    val newAccState = line match {
      case "" => {
        (Map[String, String](), currentItemPropertiesAccumulator :: itemsSoFar)
      }
      case documentPropertiesLine => {
        val documentProperties = documentPropertiesLine.split(" ")
        val currentLineKeyValuePairs = documentProperties.map(x => {
          val keyValuePair = x.split(":")
          (keyValuePair(0), keyValuePair(1))
        })
        
        val accumulatorMapUpdate = currentLineKeyValuePairs.foldLeft(Map[String, String]()) ((props, keyValuePair) => {
          props.updated(keyValuePair._1, keyValuePair._2)
        })
        
        val updatedCurrentItemAcc = currentItemPropertiesAccumulator ++ accumulatorMapUpdate
        (updatedCurrentItemAcc, itemsSoFar)
      }
    }
    newAccState
  })

  val allItems = lastItemProperties :: items

  val documents = allItems.map(itemProperties => {
    if (!mapContainsValidProperties(itemProperties)) Invalid() 
    else if (Passport.mapContainsAllProperties(itemProperties)) {
      Passport( itemProperties("byr"), itemProperties("iyr"), itemProperties("eyr"),
                itemProperties("hgt"), itemProperties("hcl"), itemProperties("ecl"), 
                itemProperties("pid"), itemProperties("cid") )
    }
    else if (NorthPoleCredentials.mapContainsAllProperties(itemProperties)) {
      NorthPoleCredentials( itemProperties("byr"), itemProperties("iyr"), itemProperties("eyr"),
                itemProperties("hgt"), itemProperties("hcl"), itemProperties("ecl"), 
                itemProperties("pid") )
    } else Invalid()
  })

  val validDocumentsCount = documents.foldLeft(0)((validItemsCount, currentItem) => {
      val currentItemValid = currentItem match {
        case _: Passport => true
        case _: NorthPoleCredentials => true
        case _: Invalid => false
      }
      if (currentItemValid) (validItemsCount + 1) else validItemsCount
    }
  )
  
  println(validDocumentsCount)

  def mapContainsValidProperties(itemProperties: Map[String, String]): Boolean = {
    val byrValid = if (itemProperties.contains("byr")) {
      val byr = itemProperties("byr").toInt
      1920 <= byr && byr <= 2002
    } else false
    
    val iyrValid = if (itemProperties.contains("iyr")) {
      val iyr = itemProperties("iyr").toInt
      2010 <= iyr && iyr <= 2020
    } else false

    val eyrValid = if (itemProperties.contains("eyr")) {
      val eyr = itemProperties("eyr").toInt
      2020 <= eyr && eyr <= 2030
    } else false

    val hgtValid = if (itemProperties.contains("hgt")) {
      val hgt = itemProperties("hgt")
      hgt match {
        case hgt if hgt.endsWith("cm") => {
          val heightCm = hgt.take(hgt.length() - 2).toInt
          150 <= heightCm && heightCm <= 193
        }
        case hgt if hgt.endsWith("in") => {
          val heightIn = hgt.take(hgt.length() - 2).toInt
          59 <= heightIn && heightIn <= 76
        }
        case _ => false
      }
    } else false

    val hclValid = if (itemProperties.contains("hcl")) {
      val hcl = itemProperties("hcl")
      val pattern = new Regex("^#[0-9a-f]{6}$")
      val matches = pattern findAllIn hcl
      matches.length == 1
    } else false

    val eclValid = if (itemProperties.contains("ecl")) {
      val ecl = itemProperties("ecl")
      val validColours = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      validColours.contains(ecl)
    } else false

    val pidValid = if (itemProperties.contains("pid")) {
      val pid = itemProperties("pid")
      val pidPattern = new Regex("^[0-9]{9}$")
      val pidMatches = pidPattern findAllIn pid
      pidMatches.length == 1
    } else false

    val allRequiredFieldsValid = byrValid && iyrValid && eyrValid && hgtValid && hclValid && eclValid && pidValid
    allRequiredFieldsValid
  }
}

