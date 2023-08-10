package com.traveltime.solutions

object Day5 {
  case class Point(x: Int, y: Int)
  case class Vent(start: Point, end: Point)

  private def optionTraverse[A](input: List[Option[A]]): Option[List[A]] =
    input.foldRight[Option[List[A]]](Some(Nil))((oa, acc) =>
      for {
        a <- oa
        list <- acc
      } yield a :: list
    )

  private def parseInput(input: String): Option[List[Vent]] = {
    optionTraverse(
      input
        .split('\n')
        .map(line => parseLine(line).flatMap(vent => Some(vent)))
        .toList
    )
  }

  def parseLine(line: String): Option[Vent] = {
    val Array(startStr, endStr) = line.split(" -> ")
    for {
      s <- parsePoint(startStr)
      e <- parsePoint(endStr)
    } yield Vent(s, e)
  }

  private def parsePoint(pointStr: String): Option[Point] = {
    val Array(xStr, yStr) = pointStr.split(",")
    for {
      xInt <- xStr.toIntOption
      yInt <- yStr.toIntOption
    } yield Point(xInt, yInt)
  }

  def part1(input: Option[List[Vent]]): Option[Int] = {
    val allPoints = for {
      vents <- input
      filteredVents = vents.filter { vent =>
        vent.start.x == vent.end.x || vent.start.y == vent.end.y
      }

      allPoints = filteredVents.flatMap { vent =>
        if (vent.start.x == vent.end.x) {
          Range
            .inclusive(vent.start.y min vent.end.y, vent.start.y max vent.end.y)
            .map(value => (vent.start.x, value))
            .toList
        } else {
          Range
            .inclusive(vent.start.x min vent.end.x, vent.start.x max vent.end.x)
            .map(value => (value, vent.start.y))
            .toList
        }
      }
    } yield allPoints

    val pointsThatAppearNotOnce = for {
      points <- allPoints
      filtered = points.filter(point => points.count(_ == point) != 1)
    } yield filtered.distinct

    pointsThatAppearNotOnce.map(x => x.length)
  }

  def part2(input: Option[List[Vent]]): Option[Int] = {
    val allPoints = for {
      vents <- input
      allPoints = vents.flatMap { vent =>
        if (vent.start.x == vent.end.x && vent.start.y != vent.end.y) {
          Range
            .inclusive(vent.start.y min vent.end.y, vent.start.y max vent.end.y)
            .map(value => Point(vent.start.x, value))
            .toList

        } else if (vent.start.y == vent.end.y && vent.start.x != vent.end.x) {
          Range
            .inclusive(vent.start.x min vent.end.x, vent.start.x max vent.end.x)
            .map(value => Point(value, vent.start.y))
            .toList

        } else if (vent.start.x == vent.start.y && vent.end.x == vent.end.y) {
          val range = Range.inclusive(vent.start.x, vent.end.x)

          range.zip(range).map(x => Point(x._1, x._2)).toList

        } else {
          val range1 =
            if (vent.start.x <= vent.end.x)
              Range.inclusive(
                vent.start.x min vent.end.x,
                vent.start.x max vent.end.x
              )
            else
              Range
                .inclusive(
                  vent.start.x min vent.end.x,
                  vent.start.x max vent.end.x
                )
                .reverse

          val range2 =
            if (vent.start.y <= vent.end.y)
              Range.inclusive(
                vent.start.y min vent.end.y,
                vent.start.y max vent.end.y
              )
            else
              Range
                .inclusive(
                  vent.start.y min vent.end.y,
                  vent.start.y max vent.end.y
                )
                .reverse

          range1.zip(range2).map(x => Point(x._1, x._2)).toList
        }
      }
    } yield allPoints

    val pointsThatAppearNotOnce = for {
      points <- allPoints
      filtered = points.filter(point => points.count(_ == point) != 1)
    } yield filtered.distinct

    pointsThatAppearNotOnce.map(x => x.length)
  }

  def main(args: Array[String]): Unit = {
    val input = """0,9 -> 5,9
                  |8,0 -> 0,8
                  |9,4 -> 3,4
                  |2,2 -> 2,1
                  |7,0 -> 7,4
                  |6,4 -> 2,0
                  |0,9 -> 2,9
                  |3,4 -> 1,4
                  |0,0 -> 8,8
                  |5,5 -> 8,2""".stripMargin

    val myInput =
      "409,872 -> 409,963\n149,412 -> 281,280\n435,281 -> 435,362\n52,208 -> 969,208\n427,265 -> 884,265\n779,741 -> 779,738\n949,41 -> 13,977\n145,690 -> 145,180\n513,665 -> 513,869\n405,174 -> 405,612\n943,504 -> 93,504\n230,808 -> 570,468\n69,278 -> 69,30\n35,336 -> 911,336\n812,83 -> 812,197\n981,962 -> 29,10\n863,709 -> 371,709\n301,963 -> 955,309\n187,101 -> 187,227\n85,762 -> 85,301\n587,362 -> 652,427\n73,359 -> 73,139\n124,449 -> 124,380\n432,659 -> 30,659\n96,728 -> 756,68\n957,215 -> 957,868\n64,779 -> 64,692\n315,403 -> 854,942\n890,663 -> 213,663\n606,864 -> 264,864\n81,446 -> 81,890\n171,463 -> 395,463\n766,639 -> 912,785\n10,163 -> 10,835\n65,906 -> 258,906\n975,364 -> 408,364\n595,728 -> 910,728\n29,274 -> 705,274\n42,965 -> 50,965\n815,588 -> 959,732\n293,344 -> 484,344\n579,220 -> 579,949\n832,951 -> 72,951\n56,296 -> 56,730\n318,589 -> 181,589\n32,33 -> 983,984\n355,794 -> 448,887\n362,25 -> 696,25\n374,207 -> 144,207\n851,66 -> 851,709\n404,98 -> 414,98\n274,601 -> 787,601\n898,100 -> 223,775\n883,638 -> 73,638\n12,989 -> 942,59\n30,590 -> 146,474\n46,711 -> 693,64\n295,925 -> 295,854\n744,89 -> 773,89\n954,55 -> 33,976\n647,361 -> 130,361\n985,410 -> 606,410\n377,884 -> 297,884\n215,961 -> 981,195\n176,422 -> 176,759\n522,216 -> 151,216\n372,149 -> 115,406\n80,92 -> 218,230\n350,754 -> 967,754\n481,944 -> 711,714\n176,795 -> 845,126\n944,568 -> 944,21\n86,302 -> 628,302\n312,691 -> 312,640\n662,874 -> 168,874\n336,98 -> 336,842\n289,69 -> 811,591\n824,777 -> 256,209\n374,311 -> 374,905\n663,254 -> 137,780\n183,248 -> 780,845\n860,32 -> 157,32\n955,794 -> 804,794\n461,179 -> 96,544\n780,264 -> 475,264\n205,583 -> 205,645\n707,364 -> 470,364\n735,364 -> 551,180\n127,479 -> 680,479\n305,162 -> 305,85\n348,349 -> 348,806\n892,94 -> 88,898\n340,593 -> 416,593\n872,175 -> 93,954\n389,750 -> 174,750\n372,661 -> 949,661\n121,256 -> 352,487\n636,204 -> 99,741\n388,328 -> 400,316\n664,333 -> 549,333\n89,92 -> 884,887\n184,373 -> 552,741\n464,949 -> 823,590\n69,713 -> 767,713\n302,33 -> 302,160\n502,110 -> 83,529\n431,331 -> 210,331\n236,588 -> 931,588\n931,315 -> 406,315\n154,272 -> 49,272\n723,159 -> 368,159\n397,615 -> 402,620\n431,796 -> 134,499\n378,837 -> 378,670\n582,28 -> 582,789\n374,967 -> 445,896\n723,874 -> 30,874\n847,767 -> 920,767\n761,101 -> 761,338\n711,833 -> 419,833\n953,335 -> 953,479\n840,614 -> 410,614\n483,485 -> 757,759\n694,472 -> 694,930\n508,275 -> 754,275\n815,419 -> 700,534\n399,843 -> 399,804\n909,771 -> 95,771\n796,893 -> 332,429\n833,488 -> 833,899\n478,240 -> 478,718\n343,720 -> 343,285\n320,204 -> 320,213\n410,794 -> 240,964\n848,227 -> 214,227\n49,919 -> 880,88\n273,648 -> 187,648\n746,313 -> 746,590\n715,527 -> 307,935\n630,771 -> 630,815\n780,670 -> 710,670\n811,789 -> 964,789\n397,267 -> 397,769\n496,510 -> 744,758\n314,62 -> 89,62\n217,417 -> 217,865\n680,862 -> 680,739\n150,902 -> 816,236\n504,120 -> 45,120\n79,43 -> 967,931\n584,197 -> 532,197\n893,717 -> 489,313\n609,759 -> 684,684\n146,24 -> 386,24\n704,645 -> 866,645\n24,329 -> 44,309\n417,741 -> 176,982\n12,48 -> 12,138\n522,359 -> 937,359\n165,269 -> 55,159\n207,537 -> 610,940\n364,213 -> 323,213\n144,750 -> 144,26\n622,840 -> 488,706\n909,201 -> 794,201\n959,185 -> 579,565\n848,720 -> 848,322\n81,159 -> 81,652\n98,174 -> 841,917\n364,437 -> 364,568\n884,90 -> 973,90\n962,931 -> 60,29\n915,967 -> 681,733\n853,859 -> 853,809\n630,362 -> 630,161\n848,455 -> 672,279\n50,212 -> 419,212\n415,719 -> 413,719\n818,274 -> 813,274\n206,779 -> 206,197\n251,729 -> 715,729\n981,873 -> 452,344\n945,708 -> 704,467\n875,38 -> 76,837\n163,799 -> 435,799\n300,821 -> 913,208\n978,15 -> 39,954\n724,167 -> 191,167\n342,600 -> 342,405\n190,17 -> 945,772\n844,659 -> 176,659\n591,135 -> 50,135\n252,353 -> 252,527\n389,906 -> 389,861\n504,387 -> 39,852\n108,567 -> 108,148\n60,600 -> 60,658\n903,968 -> 115,180\n633,34 -> 325,342\n186,536 -> 708,14\n588,276 -> 74,790\n596,837 -> 596,909\n619,278 -> 737,160\n368,953 -> 245,830\n112,496 -> 112,801\n181,583 -> 243,521\n522,293 -> 616,293\n389,166 -> 418,166\n792,940 -> 622,940\n159,953 -> 769,343\n857,231 -> 857,553\n339,176 -> 339,463\n35,412 -> 35,883\n478,694 -> 478,325\n741,257 -> 741,785\n154,130 -> 810,786\n869,81 -> 869,80\n118,815 -> 918,815\n941,954 -> 941,940\n987,51 -> 56,982\n243,571 -> 243,385\n36,138 -> 36,24\n28,971 -> 975,24\n945,842 -> 759,842\n474,470 -> 784,470\n918,520 -> 512,114\n836,606 -> 668,774\n557,918 -> 422,918\n925,889 -> 875,939\n14,317 -> 467,770\n638,312 -> 440,312\n139,353 -> 212,353\n690,339 -> 622,339\n904,863 -> 307,266\n302,939 -> 148,939\n493,409 -> 93,809\n185,871 -> 546,871\n25,355 -> 889,355\n355,832 -> 273,832\n918,56 -> 38,936\n950,189 -> 81,189\n311,469 -> 289,469\n567,269 -> 963,269\n958,306 -> 958,852\n847,753 -> 803,753\n359,983 -> 332,956\n885,128 -> 164,128\n479,841 -> 350,841\n850,280 -> 163,967\n784,822 -> 555,822\n607,29 -> 245,391\n293,280 -> 293,685\n273,716 -> 273,327\n974,155 -> 974,415\n419,34 -> 419,898\n543,344 -> 543,126\n137,163 -> 737,163\n252,158 -> 899,805\n581,36 -> 857,36\n588,728 -> 588,511\n320,303 -> 706,303\n180,914 -> 939,155\n547,676 -> 547,535\n974,961 -> 97,84\n179,757 -> 789,757\n450,706 -> 450,452\n595,598 -> 526,598\n184,42 -> 184,43\n221,963 -> 738,963\n79,976 -> 79,109\n638,793 -> 638,986\n98,81 -> 534,517\n700,334 -> 700,201\n533,265 -> 76,265\n131,839 -> 728,839\n120,78 -> 815,773\n455,825 -> 165,825\n521,258 -> 978,258\n425,931 -> 966,931\n358,754 -> 506,902\n126,228 -> 851,228\n393,114 -> 29,114\n58,615 -> 425,615\n384,607 -> 581,804\n140,939 -> 140,673\n372,400 -> 795,400\n115,165 -> 84,165\n788,275 -> 544,275\n126,329 -> 725,329\n182,541 -> 99,541\n410,709 -> 974,709\n681,202 -> 687,208\n495,832 -> 733,594\n259,781 -> 444,596\n49,442 -> 49,243\n988,986 -> 18,16\n224,624 -> 224,32\n755,41 -> 185,611\n512,168 -> 492,168\n55,891 -> 762,184\n162,845 -> 162,19\n976,771 -> 449,244\n398,789 -> 398,336\n796,28 -> 796,326\n589,735 -> 589,668\n382,303 -> 10,675\n650,968 -> 140,458\n784,265 -> 245,804\n961,980 -> 346,365\n685,360 -> 567,360\n60,847 -> 749,158\n828,80 -> 46,862\n96,760 -> 96,340\n22,372 -> 878,372\n26,973 -> 928,71\n86,697 -> 86,790\n684,896 -> 684,638\n644,370 -> 644,177\n915,854 -> 134,73\n453,678 -> 453,394\n716,594 -> 160,38\n757,62 -> 518,301\n977,928 -> 977,53\n705,858 -> 328,858\n352,527 -> 759,120\n221,365 -> 677,821\n237,815 -> 829,815\n756,615 -> 756,129\n681,948 -> 28,295\n712,731 -> 120,731\n141,292 -> 141,338\n874,781 -> 874,414\n197,892 -> 972,117\n673,779 -> 113,219\n469,240 -> 887,240\n546,367 -> 270,367\n115,356 -> 264,505\n587,969 -> 942,969\n697,332 -> 432,597\n251,742 -> 251,402\n655,258 -> 875,38\n806,801 -> 438,801\n776,525 -> 296,45\n437,137 -> 437,507\n310,870 -> 875,305\n92,877 -> 949,20\n96,695 -> 269,868\n917,601 -> 917,347\n598,172 -> 91,679\n407,743 -> 818,743\n74,932 -> 720,286\n574,967 -> 747,794\n989,982 -> 22,15\n128,514 -> 590,976\n46,86 -> 851,891\n976,914 -> 87,25\n948,659 -> 948,40\n442,504 -> 442,398\n310,824 -> 509,625\n946,338 -> 433,851\n555,241 -> 531,265\n336,13 -> 336,48\n688,880 -> 394,880\n105,143 -> 823,861\n761,330 -> 228,863\n405,132 -> 853,580\n780,886 -> 326,432\n471,45 -> 471,600\n810,971 -> 810,418\n591,58 -> 591,821\n494,133 -> 616,11\n519,746 -> 519,230\n696,879 -> 107,879\n21,24 -> 937,940\n18,822 -> 637,203\n159,468 -> 570,468\n635,850 -> 635,408\n446,469 -> 337,469\n962,690 -> 962,28\n925,958 -> 40,73\n123,183 -> 545,183\n26,767 -> 26,548\n898,101 -> 249,750\n85,706 -> 85,446\n224,763 -> 15,763\n503,655 -> 847,655\n659,905 -> 650,905\n596,462 -> 851,207\n421,230 -> 884,230\n550,18 -> 524,18\n437,375 -> 715,375\n210,202 -> 210,348\n263,298 -> 263,948\n132,352 -> 749,969\n582,454 -> 582,771\n724,99 -> 724,388\n16,528 -> 16,317\n22,949 -> 22,987\n457,328 -> 457,327\n279,130 -> 279,234\n56,338 -> 368,650\n448,295 -> 533,295\n898,90 -> 60,928\n116,619 -> 208,527\n614,481 -> 614,152\n113,412 -> 543,412\n854,305 -> 854,748\n225,825 -> 115,935\n976,365 -> 522,365\n547,394 -> 547,82\n800,695 -> 800,111\n36,10 -> 973,947\n557,441 -> 527,441\n62,798 -> 708,798\n582,585 -> 816,585\n599,33 -> 70,562\n971,863 -> 971,150\n408,273 -> 535,273\n797,215 -> 692,215\n537,922 -> 519,922\n299,302 -> 504,302\n88,361 -> 88,249\n430,311 -> 544,311\n902,127 -> 902,791\n346,986 -> 346,307\n523,534 -> 285,296\n600,628 -> 676,628\n373,689 -> 945,117\n265,189 -> 343,189\n391,632 -> 391,728\n500,516 -> 712,304\n250,891 -> 41,891\n591,539 -> 591,577\n968,250 -> 265,250\n80,32 -> 80,988\n668,385 -> 323,385\n407,103 -> 865,103\n755,190 -> 693,190\n754,113 -> 754,392\n957,261 -> 967,251\n881,734 -> 803,812\n234,912 -> 234,326\n711,893 -> 711,29\n513,479 -> 782,479\n111,224 -> 675,224\n125,645 -> 125,502\n591,983 -> 591,726\n462,390 -> 85,390\n854,275 -> 649,480\n253,792 -> 23,792\n940,975 -> 447,975\n604,297 -> 604,386\n172,217 -> 633,217\n159,941 -> 159,569\n468,72 -> 468,787\n931,935 -> 36,40\n114,166 -> 360,412\n539,740 -> 349,930\n314,342 -> 65,93\n905,650 -> 817,650\n786,546 -> 173,546\n209,967 -> 209,142\n679,64 -> 68,675\n472,70 -> 840,70\n750,753 -> 124,753\n79,490 -> 652,490\n655,114 -> 655,575\n984,18 -> 984,287\n458,41 -> 961,544\n358,208 -> 358,679\n977,830 -> 283,136\n250,800 -> 187,800\n64,516 -> 64,809\n196,235 -> 781,235\n727,988 -> 891,824\n437,158 -> 437,549\n565,231 -> 565,368\n293,411 -> 29,147\n100,905 -> 969,36\n59,615 -> 920,615\n844,883 -> 844,173\n966,980 -> 25,39\n802,923 -> 163,284\n980,24 -> 730,24\n357,474 -> 32,799\n735,23 -> 735,566\n970,426 -> 970,42\n194,542 -> 194,328\n372,69 -> 630,69\n779,295 -> 239,835\n793,381 -> 650,238\n366,579 -> 366,762\n502,354 -> 530,326\n432,125 -> 432,795\n162,220 -> 162,239\n631,944 -> 63,944\n261,526 -> 330,457\n913,445 -> 913,958\n786,160 -> 333,613\n919,123 -> 88,954"

    val vents = parseInput(input)

    println(part1(vents))
  }
}
