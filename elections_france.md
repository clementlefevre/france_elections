Elections France 2017
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringr)
library(GGally)
```

    ## 
    ## Attaching package: 'GGally'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     nasa

``` r
library("sp")
library("rgdal")
```

    ## rgdal: version: 1.2-6, (SVN revision 651)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 1.11.3, released 2015/09/16
    ##  Path to GDAL shared files: /usr/share/gdal/1.11
    ##  Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
    ##  Path to PROJ.4 shared files: (autodetected)
    ##  Linking to sp version: 1.2-4

``` r
library("RColorBrewer")
library("classInt")


library("ggmap")
```

    ## Loading required package: ggplot2

``` r
library("maptools")
```

    ## Checking rgeos availability: FALSE
    ##      Note: when rgeos is not available, polygon geometry     computations in maptools depend on gpclib,
    ##      which has a restricted licence. It is disabled by default;
    ##      to enable gpclib, type gpclibPermit()

``` r
df_chomage<-read.csv('data/chomages_France.csv',sep=';',stringsAsFactors = F,skip = 1)

df_chomage$ZE2010<- df_chomage$ZE2010 %>% as.numeric()
```

    ## Warning in function_list[[k]](value): NAs introduced by coercion

``` r
df_zones_communes<- read.csv('data/code_geo_communes.csv',sep=';',stringsAsFactors = 1)
df_pres<-read.csv('data/Pres 2012.csv',stringsAsFactors = F,sep=';')
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

``` r
sample_communes<-sample_n(df_zones_communes,50) %>% select(LIBGEO) %>% .$LIBGEO
sample_pres<-sample_n(df_pres,100) %>% select(Libellé.de.la.commune)%>% .$Libellé.de.la.commune
```

``` r
df_pres %>% filter (Libellé.de.la.commune %in% sample_pres)
```

    ##     Code.du.département Libellé.du.département Code.de.la.commune
    ## 1                     1                    AIN                122
    ## 2                     2                  AISNE                 97
    ## 3                     2                  AISNE                271
    ## 4                     2                  AISNE                518
    ## 5                     3                 ALLIER                139
    ## 6                     3                 ALLIER                222
    ## 7                     3                 ALLIER                241
    ## 8                     6        ALPES MARITIMES                 75
    ## 9                     7                ARDECHE                314
    ## 10                    9                 ARIEGE                173
    ## 11                    9                 ARIEGE                289
    ## 12                   10                   AUBE                121
    ## 13                   10                   AUBE                154
    ## 14                   11                   AUDE                110
    ## 15                   11                   AUDE                261
    ## 16                   11                   AUDE                282
    ## 17                   12                AVEYRON                 36
    ## 18                   12                AVEYRON                138
    ## 19                   14               CALVADOS                217
    ## 20                   16               CHARENTE                358
    ## 21                   17      CHARENTE MARITIME                 23
    ## 22                   17      CHARENTE MARITIME                355
    ## 23                   18                   CHER                201
    ## 24                   2B            HAUTE CORSE                238
    ## 25                   21              COTE D'OR                121
    ## 26                   22          COTES D'ARMOR                187
    ## 27                   22          COTES D'ARMOR                256
    ## 28                   22          COTES D'ARMOR                278
    ## 29                   27                   EURE                  2
    ## 30                   27                   EURE                 81
    ## 31                   28           EURE ET LOIR                 50
    ## 32                   28           EURE ET LOIR                226
    ## 33                   28           EURE ET LOIR                275
    ## 34                   28           EURE ET LOIR                407
    ## 35                   29              FINISTERE                294
    ## 36                   30                   GARD                 13
    ## 37                   30                   GARD                 22
    ## 38                   30                   GARD                 99
    ## 39                   30                   GARD                205
    ## 40                   31          HAUTE GARONNE                182
    ## 41                   31          HAUTE GARONNE                239
    ## 42                   32                   GERS                  7
    ## 43                   32                   GERS                326
    ## 44                   32                   GERS                467
    ## 45                   33                GIRONDE                250
    ## 46                   38                  ISERE                105
    ## 47                   38                  ISERE                261
    ## 48                   39                   JURA                 36
    ## 49                   39                   JURA                136
    ## 50                   39                   JURA                190
    ## 51                   39                   JURA                220
    ## 52                   39                   JURA                342
    ## 53                   41           LOIR ET CHER                  1
    ## 54                   41           LOIR ET CHER                166
    ## 55                   42                  LOIRE                 98
    ## 56                   42                  LOIRE                156
    ## 57                   42                  LOIRE                306
    ## 58                   44       LOIRE ATLANTIQUE                 48
    ## 59                   44       LOIRE ATLANTIQUE                210
    ## 60                   46                    LOT                250
    ## 61                   47         LOT ET GARONNE                 11
    ## 62                   47         LOT ET GARONNE                309
    ## 63                   48                 LOZERE                 66
    ## 64                   50                 MANCHE                192
    ## 65                   50                 MANCHE                318
    ## 66                   51                  MARNE                621
    ## 67                   52            HAUTE MARNE                163
    ## 68                   53                MAYENNE                138
    ## 69                   54     MEURTHE ET MOSELLE                 82
    ## 70                   54     MEURTHE ET MOSELLE                172
    ## 71                   55                  MEUSE                 44
    ## 72                   55                  MEUSE                323
    ## 73                   55                  MEUSE                544
    ## 74                   58                 NIEVRE                 14
    ## 75                   58                 NIEVRE                 43
    ## 76                   58                 NIEVRE                162
    ## 77                   58                 NIEVRE                284
    ## 78                   59                   NORD                 37
    ## 79                   59                   NORD                361
    ## 80                   60                   OISE                387
    ## 81                   61                   ORNE                451
    ## 82                   62          PAS DE CALAIS                 84
    ## 83                   62          PAS DE CALAIS                475
    ## 84                   62          PAS DE CALAIS                676
    ## 85                   62          PAS DE CALAIS                797
    ## 86                   63            PUY DE DOME                 48
    ## 87                   63            PUY DE DOME                129
    ## 88                   64   PYRENEES ATLANTIQUES                132
    ## 89                   64   PYRENEES ATLANTIQUES                183
    ## 90                   65        HAUTES PYRENEES                217
    ## 91                   66    PYRENEES ORIENTALES                 77
    ## 92                   66    PYRENEES ORIENTALES                165
    ## 93                   69                  RHONE                138
    ## 94                   70            HAUTE SAONE                262
    ## 95                   70            HAUTE SAONE                479
    ## 96                   70            HAUTE SAONE                572
    ## 97                   71         SAONE ET LOIRE                404
    ## 98                   71         SAONE ET LOIRE                410
    ## 99                   73                 SAVOIE                117
    ## 100                  74           HAUTE SAVOIE                290
    ## 101                  76         SEINE MARITIME                 81
    ## 102                  76         SEINE MARITIME                298
    ## 103                  76         SEINE MARITIME                514
    ## 104                  77         SEINE ET MARNE                180
    ## 105                  77         SEINE ET MARNE                369
    ## 106                  78               YVELINES                238
    ## 107                  78               YVELINES                299
    ## 108                  78               YVELINES                380
    ## 109                  79            DEUX SEVRES                171
    ## 110                  80                  SOMME                 55
    ## 111                  80                  SOMME                667
    ## 112                  81                   TARN                229
    ## 113                  86                 VIENNE                235
    ## 114                  88                 VOSGES                318
    ## 115                  88                 VOSGES                363
    ## 116                  89                  YONNE                479
    ## 117                  92         HAUTS DE SEINE                 51
    ##             Libellé.de.la.commune Inscrits Abstentions X..Abs.Ins Votants
    ## 1            Cormaranche-en-Bugey      625          69      11.04     556
    ## 2                        Boncourt      205          32      15.61     173
    ## 3                        Dravegny      110          16      14.55      94
    ## 4                       Montlevon      217          38      17.51     179
    ## 5                        Laprugne      310          56      18.06     254
    ## 6                   Saint-Caprais       70          11      15.71      59
    ## 7          Saint-Léopardin-d'Augy      284          39      13.73     245
    ## 8                          Levens     3866         678      17.54    3188
    ## 9                          Silhac      319          31       9.72     288
    ## 10                        Loubens      179          21      11.73     158
    ## 11               Lorp-Sentaraille      991         133      13.42     858
    ## 12                      Dampierre      284          42      14.79     242
    ## 13            Fontenay-de-Bossery       78           8      10.26      70
    ## 14                      Coustouge       90          16      17.78      74
    ## 15                           Moux      544         105      19.30     439
    ## 16            Peyrefitte-du-Razès       52          14      26.92      38
    ## 17                        Brommat      608         109      17.93     499
    ## 18               Marcillac-Vallon     1266         151      11.93    1115
    ## 19                      Dampierre      108          15      13.89      93
    ## 20      Saint-Yrieix-sur-Charente     5245         762      14.53    4483
    ## 21                          Aujac      255          43      16.86     212
    ## 22                Sainte-Lheurine      400          76      19.00     324
    ## 23                  Saint-Caprais      505          67      13.27     438
    ## 24               Poggio-di-Venaco      153          38      24.84     115
    ## 25                 Bussy-la-Pesle       74           9      12.16      65
    ## 26                         Plérin    10166        1035      10.18    9131
    ## 27              Quemper-Guézennec      903         117      12.96     786
    ## 28                   Saint-Brieuc    31120        7166      23.03   23954
    ## 29                           Acon      356          72      20.22     284
    ## 30                       Boncourt      128          15      11.72     113
    ## 31                       Boncourt      219          23      10.50     196
    ## 32                     Maillebois      734         123      16.76     611
    ## 33                          Néron      524          93      17.75     431
    ## 34                       Vichères      250          34      13.60     216
    ## 35                      Le Tréhou      430          57      13.26     373
    ## 36                     Argilliers      249          28      11.24     221
    ## 37                          Aujac      187          30      16.04     157
    ## 38                           Cros      229          27      11.79     202
    ## 39                  Pougnadoresse      172          22      12.79     150
    ## 40                     Fenouillet     3581         495      13.82    3086
    ## 41                L'Isle-en-Dodon     1398         185      13.23    1213
    ## 42                        Ardizas      162          22      13.58     140
    ## 43                      Pouylebon      133          32      24.06     101
    ## 44                  Saint-Caprais      102          12      11.76      90
    ## 45                        Loubens      262          42      16.03     220
    ## 46                        Chirens     1725         285      16.52    1440
    ## 47                       Morestel     2448         435      17.77    2013
    ## 48               Balme-d'Epy (La)       43           9      20.93      34
    ## 49                       Chemenot       31           2       6.45      29
    ## 50                      Dampierre      813         127      15.62     686
    ## 51                      Falletans      294          27       9.18     267
    ## 52                          Monay      116          21      18.10      95
    ## 53                         Ambloy      142          20      14.08     122
    ## 54                          Oisly      290          36      12.41     254
    ## 55                      Fourneaux      475          55      11.58     420
    ## 56                        Neulise      902         138      15.30     764
    ## 57                     Tarentaise      379          49      12.93     330
    ## 58                         Couffé     1678         207      12.34    1471
    ## 59                        Trignac     5260         975      18.54    4285
    ## 60                  Saint-Caprais       74           8      10.81      66
    ## 61                          Anthé      139          23      16.55     116
    ## 62                     Tombeboeuf      336          50      14.88     286
    ## 63           Fraissinet-de-Lozère      196          40      20.41     156
    ## 64                      Fourneaux       81          15      18.52      66
    ## 65             Le Mesnil-Rainfray      155          20      12.90     135
    ## 66              Vienne-le-Château      411          75      18.25     336
    ## 67                      Dampierre      303          68      22.44     235
    ## 68                     Longuefuye      244          38      15.57     206
    ## 69                       Boncourt      164          16       9.76     148
    ## 70          Doncourt-lès-Longuyon      188          27      14.36     161
    ## 71                        Belrain       32           3       9.38      29
    ## 72          Martincourt-sur-Meuse       46           3       6.52      43
    ## 73                       Velosnes       83          23      27.71      60
    ## 74                      Arzembouy       78          21      26.92      57
    ## 75                 Bussy-la-Pesle       41           6      14.63      35
    ## 76                     Menestreau      112          24      21.43      88
    ## 77                          Talon       55           8      14.55      47
    ## 78             Avesnes-les-Aubert     2605         477      18.31    2128
    ## 79                       Lourches     2176         526      24.17    1650
    ## 80        Marseille-en-Beauvaisis      862         150      17.40     712
    ## 81  Saint-Quentin-les-Chardonnets      232          32      13.79     200
    ## 82                          Barly      193          29      15.03     164
    ## 83                        Ivergny      174          21      12.07     153
    ## 84                        Quernes      369          33       8.94     336
    ## 85                      Siracourt      221          30      13.57     191
    ## 86                   Bourg-Lastic      768         120      15.63     648
    ## 87                           Cros      177          32      18.08     145
    ## 88                        Bizanos     3630         596      16.42    3034
    ## 89                   Caubios-Loos      401          39       9.73     362
    ## 90                       Hautaget       45           5      11.11      40
    ## 91                     Fenouillet       76          20      26.32      56
    ## 92                          Rodès      470          66      14.04     404
    ## 93                     Montromant      318          44      13.84     274
    ## 94                   Genevreuille      135           8       5.93     127
    ## 95             Sauvigney-lès-Gray       95           8       8.42      87
    ## 96               Vitrey-sur-Mance      243          49      20.16     194
    ## 97                   Saint-Désert      704         104      14.77     600
    ## 98        Saint-Etienne-en-Bresse      592          99      16.72     493
    ## 99                      Fourneaux      550         103      18.73     447
    ## 100                    Vallorcine      341          42      12.32     299
    ## 101             Berneval-le-Grand     1017         174      17.11     843
    ## 102                    Ganzeville      414          71      17.15     343
    ## 103        Quévreville-la-Poterie      805         106      13.17     699
    ## 104              Férolles-Attilly      869         202      23.25     667
    ## 105                        Poincy      534          68      12.73     466
    ## 106               Flins-sur-Seine     1603         297      18.53    1306
    ## 107                   Hardricourt     1365         242      17.73    1123
    ## 108                         Maule     4220         861      20.40    3359
    ## 109              Mauzé-Thouarsais     1831         354      19.33    1477
    ## 110                         Barly      142          23      16.20     119
    ## 111                     Remaugies       95          14      14.74      81
    ## 112                    Roquevidal      108          10       9.26      98
    ## 113      Saint-Maurice-la-Clouère      930         126      13.55     804
    ## 114                      Moyemont      171          30      17.54     141
    ## 115                       Punerot      142          34      23.94     108
    ## 116                   Vincelottes      278          51      18.35     227
    ## 117             Neuilly-sur-Seine    37003        5964      16.12   31039
    ##     X..Vot.Ins Blancs.et.nuls X..BlNuls.Ins X..BlNuls.Vot Exprimés
    ## 1        88.96             17          2.72          3.06      539
    ## 2        84.39              0          0.00          0.00      173
    ## 3        85.45              1          0.91          1.06       93
    ## 4        82.49              4          1.84          2.23      175
    ## 5        81.94              6          1.94          2.36      248
    ## 6        84.29              0          0.00          0.00       59
    ## 7        86.27              5          1.76          2.04      240
    ## 8        82.46             50          1.29          1.57     3138
    ## 9        90.28              1          0.31          0.35      287
    ## 10       88.27              5          2.79          3.16      153
    ## 11       86.58             11          1.11          1.28      847
    ## 12       85.21              4          1.41          1.65      238
    ## 13       89.74              2          2.56          2.86       68
    ## 14       82.22              2          2.22          2.70       72
    ## 15       80.70              9          1.65          2.05      430
    ## 16       73.08              1          1.92          2.63       37
    ## 17       82.07              6          0.99          1.20      493
    ## 18       88.07             18          1.42          1.61     1097
    ## 19       86.11              0          0.00          0.00       93
    ## 20       85.47             68          1.30          1.52     4415
    ## 21       83.14              1          0.39          0.47      211
    ## 22       81.00             11          2.75          3.40      313
    ## 23       86.73              6          1.19          1.37      432
    ## 24       75.16              2          1.31          1.74      113
    ## 25       87.84              3          4.05          4.62       62
    ## 26       89.82            162          1.59          1.77     8969
    ## 27       87.04             13          1.44          1.65      773
    ## 28       76.97            347          1.12          1.45    23607
    ## 29       79.78              7          1.97          2.46      277
    ## 30       88.28              2          1.56          1.77      111
    ## 31       89.50              4          1.83          2.04      192
    ## 32       83.24             12          1.63          1.96      599
    ## 33       82.25              3          0.57          0.70      428
    ## 34       86.40              3          1.20          1.39      213
    ## 35       86.74              2          0.47          0.54      371
    ## 36       88.76              3          1.20          1.36      218
    ## 37       83.96              5          2.67          3.18      152
    ## 38       88.21              2          0.87          0.99      200
    ## 39       87.21              2          1.16          1.33      148
    ## 40       86.18             54          1.51          1.75     3032
    ## 41       86.77             27          1.93          2.23     1186
    ## 42       86.42              2          1.23          1.43      138
    ## 43       75.94              2          1.50          1.98       99
    ## 44       88.24              2          1.96          2.22       88
    ## 45       83.97              9          3.44          4.09      211
    ## 46       83.48             20          1.16          1.39     1420
    ## 47       82.23             27          1.10          1.34     1986
    ## 48       79.07              0          0.00          0.00       34
    ## 49       93.55              2          6.45          6.90       27
    ## 50       84.38             12          1.48          1.75      674
    ## 51       90.82              5          1.70          1.87      262
    ## 52       81.90              3          2.59          3.16       92
    ## 53       85.92              4          2.82          3.28      118
    ## 54       87.59              2          0.69          0.79      252
    ## 55       88.42             13          2.74          3.10      407
    ## 56       84.70             20          2.22          2.62      744
    ## 57       87.07             11          2.90          3.33      319
    ## 58       87.66             37          2.21          2.52     1434
    ## 59       81.46             72          1.37          1.68     4213
    ## 60       89.19              1          1.35          1.52       65
    ## 61       83.45              3          2.16          2.59      113
    ## 62       85.12              9          2.68          3.15      277
    ## 63       79.59              4          2.04          2.56      152
    ## 64       81.48              0          0.00          0.00       66
    ## 65       87.10              5          3.23          3.70      130
    ## 66       81.75              5          1.22          1.49      331
    ## 67       77.56              4          1.32          1.70      231
    ## 68       84.43              4          1.64          1.94      202
    ## 69       90.24              1          0.61          0.68      147
    ## 70       85.64              3          1.60          1.86      158
    ## 71       90.63              1          3.13          3.45       28
    ## 72       93.48              2          4.35          4.65       41
    ## 73       72.29              1          1.20          1.67       59
    ## 74       73.08              2          2.56          3.51       55
    ## 75       85.37              1          2.44          2.86       34
    ## 76       78.57              1          0.89          1.14       87
    ## 77       85.45              0          0.00          0.00       47
    ## 78       81.69             38          1.46          1.79     2090
    ## 79       75.83             33          1.52          2.00     1617
    ## 80       82.60             16          1.86          2.25      696
    ## 81       86.21              3          1.29          1.50      197
    ## 82       84.97              4          2.07          2.44      160
    ## 83       87.93              2          1.15          1.31      151
    ## 84       91.06              4          1.08          1.19      332
    ## 85       86.43              3          1.36          1.57      188
    ## 86       84.38             10          1.30          1.54      638
    ## 87       81.92              3          1.69          2.07      142
    ## 88       83.58             57          1.57          1.88     2977
    ## 89       90.27              8          2.00          2.21      354
    ## 90       88.89              1          2.22          2.50       39
    ## 91       73.68              1          1.32          1.79       55
    ## 92       85.96              6          1.28          1.49      398
    ## 93       86.16              5          1.57          1.82      269
    ## 94       94.07              2          1.48          1.57      125
    ## 95       91.58              1          1.05          1.15       86
    ## 96       79.84              7          2.88          3.61      187
    ## 97       85.23             14          1.99          2.33      586
    ## 98       83.28              6          1.01          1.22      487
    ## 99       81.27             12          2.18          2.68      435
    ## 100      87.68              6          1.76          2.01      293
    ## 101      82.89             16          1.57          1.90      827
    ## 102      82.85              7          1.69          2.04      336
    ## 103      86.83             14          1.74          2.00      685
    ## 104      76.75              7          0.81          1.05      660
    ## 105      87.27              7          1.31          1.50      459
    ## 106      81.47             31          1.93          2.37     1275
    ## 107      82.27             17          1.25          1.51     1106
    ## 108      79.60             48          1.14          1.43     3311
    ## 109      80.67             41          2.24          2.78     1436
    ## 110      83.80              2          1.41          1.68      117
    ## 111      85.26              1          1.05          1.23       80
    ## 112      90.74              3          2.78          3.06       95
    ## 113      86.45             13          1.40          1.62      791
    ## 114      82.46              2          1.17          1.42      139
    ## 115      76.06              2          1.41          1.85      106
    ## 116      81.65              5          1.80          2.20      222
    ## 117      83.88            257          0.69          0.83    30782
    ##     X..Exp.Ins X..Exp.Vot  Sexe  Nom Prénom Voix X..Voix.Ins X..Voix.Exp
    ## 1        86.24      96.94 FALSE JOLY    Eva   11        1.76        2.04
    ## 2        84.39     100.00 FALSE JOLY    Eva    0        0.00        0.00
    ## 3        84.55      98.94 FALSE JOLY    Eva    1        0.91        1.08
    ## 4        80.65      97.77 FALSE JOLY    Eva    5        2.30        2.86
    ## 5        80.00      97.64 FALSE JOLY    Eva    5        1.61        2.02
    ## 6        84.29     100.00 FALSE JOLY    Eva    2        2.86        3.39
    ## 7        84.51      97.96 FALSE JOLY    Eva    3        1.06        1.25
    ## 8        81.17      98.43 FALSE JOLY    Eva   70        1.81        2.23
    ## 9        89.97      99.65 FALSE JOLY    Eva   15        4.70        5.23
    ## 10       85.47      96.84 FALSE JOLY    Eva    9        5.03        5.88
    ## 11       85.47      98.72 FALSE JOLY    Eva   13        1.31        1.53
    ## 12       83.80      98.35 FALSE JOLY    Eva    2        0.70        0.84
    ## 13       87.18      97.14 FALSE JOLY    Eva    0        0.00        0.00
    ## 14       80.00      97.30 FALSE JOLY    Eva    1        1.11        1.39
    ## 15       79.04      97.95 FALSE JOLY    Eva   11        2.02        2.56
    ## 16       71.15      97.37 FALSE JOLY    Eva    1        1.92        2.70
    ## 17       81.09      98.80 FALSE JOLY    Eva    5        0.82        1.01
    ## 18       86.65      98.39 FALSE JOLY    Eva   26        2.05        2.37
    ## 19       86.11     100.00 FALSE JOLY    Eva    1        0.93        1.08
    ## 20       84.18      98.48 FALSE JOLY    Eva  103        1.96        2.33
    ## 21       82.75      99.53 FALSE JOLY    Eva    5        1.96        2.37
    ## 22       78.25      96.60 FALSE JOLY    Eva    4        1.00        1.28
    ## 23       85.54      98.63 FALSE JOLY    Eva    4        0.79        0.93
    ## 24       73.86      98.26 FALSE JOLY    Eva    5        3.27        4.42
    ## 25       83.78      95.38 FALSE JOLY    Eva    2        2.70        3.23
    ## 26       88.23      98.23 FALSE JOLY    Eva  315        3.10        3.51
    ## 27       85.60      98.35 FALSE JOLY    Eva   23        2.55        2.98
    ## 28       75.86      98.55 FALSE JOLY    Eva  901        2.90        3.82
    ## 29       77.81      97.54 FALSE JOLY    Eva    1        0.28        0.36
    ## 30       86.72      98.23 FALSE JOLY    Eva    1        0.78        0.90
    ## 31       87.67      97.96 FALSE JOLY    Eva   11        5.02        5.73
    ## 32       81.61      98.04 FALSE JOLY    Eva   16        2.18        2.67
    ## 33       81.68      99.30 FALSE JOLY    Eva   13        2.48        3.04
    ## 34       85.20      98.61 FALSE JOLY    Eva    6        2.40        2.82
    ## 35       86.28      99.46 FALSE JOLY    Eva    9        2.09        2.43
    ## 36       87.55      98.64 FALSE JOLY    Eva    7        2.81        3.21
    ## 37       81.28      96.82 FALSE JOLY    Eva   13        6.95        8.55
    ## 38       87.34      99.01 FALSE JOLY    Eva   24       10.48       12.00
    ## 39       86.05      98.67 FALSE JOLY    Eva    1        0.58        0.68
    ## 40       84.67      98.25 FALSE JOLY    Eva   60        1.68        1.98
    ## 41       84.84      97.77 FALSE JOLY    Eva   18        1.29        1.52
    ## 42       85.19      98.57 FALSE JOLY    Eva    4        2.47        2.90
    ## 43       74.44      98.02 FALSE JOLY    Eva    6        4.51        6.06
    ## 44       86.27      97.78 FALSE JOLY    Eva    5        4.90        5.68
    ## 45       80.53      95.91 FALSE JOLY    Eva    7        2.67        3.32
    ## 46       82.32      98.61 FALSE JOLY    Eva   54        3.13        3.80
    ## 47       81.13      98.66 FALSE JOLY    Eva   33        1.35        1.66
    ## 48       79.07     100.00 FALSE JOLY    Eva    1        2.33        2.94
    ## 49       87.10      93.10 FALSE JOLY    Eva    1        3.23        3.70
    ## 50       82.90      98.25 FALSE JOLY    Eva   10        1.23        1.48
    ## 51       89.12      98.13 FALSE JOLY    Eva    8        2.72        3.05
    ## 52       79.31      96.84 FALSE JOLY    Eva    6        5.17        6.52
    ## 53       83.10      96.72 FALSE JOLY    Eva    5        3.52        4.24
    ## 54       86.90      99.21 FALSE JOLY    Eva    5        1.72        1.98
    ## 55       85.68      96.90 FALSE JOLY    Eva    9        1.89        2.21
    ## 56       82.48      97.38 FALSE JOLY    Eva   12        1.33        1.61
    ## 57       84.17      96.67 FALSE JOLY    Eva   11        2.90        3.45
    ## 58       85.46      97.48 FALSE JOLY    Eva   46        2.74        3.21
    ## 59       80.10      98.32 FALSE JOLY    Eva   49        0.93        1.16
    ## 60       87.84      98.48 FALSE JOLY    Eva    3        4.05        4.62
    ## 61       81.29      97.41 FALSE JOLY    Eva    0        0.00        0.00
    ## 62       82.44      96.85 FALSE JOLY    Eva    4        1.19        1.44
    ## 63       77.55      97.44 FALSE JOLY    Eva    5        2.55        3.29
    ## 64       81.48     100.00 FALSE JOLY    Eva    0        0.00        0.00
    ## 65       83.87      96.30 FALSE JOLY    Eva    0        0.00        0.00
    ## 66       80.54      98.51 FALSE JOLY    Eva    1        0.24        0.30
    ## 67       76.24      98.30 FALSE JOLY    Eva    2        0.66        0.87
    ## 68       82.79      98.06 FALSE JOLY    Eva   13        5.33        6.44
    ## 69       89.63      99.32 FALSE JOLY    Eva    1        0.61        0.68
    ## 70       84.04      98.14 FALSE JOLY    Eva    2        1.06        1.27
    ## 71       87.50      96.55 FALSE JOLY    Eva    3        9.38       10.71
    ## 72       89.13      95.35 FALSE JOLY    Eva    0        0.00        0.00
    ## 73       71.08      98.33 FALSE JOLY    Eva    0        0.00        0.00
    ## 74       70.51      96.49 FALSE JOLY    Eva    0        0.00        0.00
    ## 75       82.93      97.14 FALSE JOLY    Eva    0        0.00        0.00
    ## 76       77.68      98.86 FALSE JOLY    Eva    4        3.57        4.60
    ## 77       85.45     100.00 FALSE JOLY    Eva    0        0.00        0.00
    ## 78       80.23      98.21 FALSE JOLY    Eva   24        0.92        1.15
    ## 79       74.31      98.00 FALSE JOLY    Eva    5        0.23        0.31
    ## 80       80.74      97.75 FALSE JOLY    Eva    9        1.04        1.29
    ## 81       84.91      98.50 FALSE JOLY    Eva    2        0.86        1.02
    ## 82       82.90      97.56 FALSE JOLY    Eva    1        0.52        0.63
    ## 83       86.78      98.69 FALSE JOLY    Eva    2        1.15        1.32
    ## 84       89.97      98.81 FALSE JOLY    Eva    1        0.27        0.30
    ## 85       85.07      98.43 FALSE JOLY    Eva    2        0.90        1.06
    ## 86       83.07      98.46 FALSE JOLY    Eva    6        0.78        0.94
    ## 87       80.23      97.93 FALSE JOLY    Eva    5        2.82        3.52
    ## 88       82.01      98.12 FALSE JOLY    Eva   70        1.93        2.35
    ## 89       88.28      97.79 FALSE JOLY    Eva    7        1.75        1.98
    ## 90       86.67      97.50 FALSE JOLY    Eva    1        2.22        2.56
    ## 91       72.37      98.21 FALSE JOLY    Eva    2        2.63        3.64
    ## 92       84.68      98.51 FALSE JOLY    Eva   17        3.62        4.27
    ## 93       84.59      98.18 FALSE JOLY    Eva   18        5.66        6.69
    ## 94       92.59      98.43 FALSE JOLY    Eva    1        0.74        0.80
    ## 95       90.53      98.85 FALSE JOLY    Eva    2        2.11        2.33
    ## 96       76.95      96.39 FALSE JOLY    Eva    2        0.82        1.07
    ## 97       83.24      97.67 FALSE JOLY    Eva    6        0.85        1.02
    ## 98       82.26      98.78 FALSE JOLY    Eva    8        1.35        1.64
    ## 99       79.09      97.32 FALSE JOLY    Eva   10        1.82        2.30
    ## 100      85.92      97.99 FALSE JOLY    Eva   33        9.68       11.26
    ## 101      81.32      98.10 FALSE JOLY    Eva   14        1.38        1.69
    ## 102      81.16      97.96 FALSE JOLY    Eva    3        0.72        0.89
    ## 103      85.09      98.00 FALSE JOLY    Eva    9        1.12        1.31
    ## 104      75.95      98.95 FALSE JOLY    Eva   13        1.50        1.97
    ## 105      85.96      98.50 FALSE JOLY    Eva    4        0.75        0.87
    ## 106      79.54      97.63 FALSE JOLY    Eva   22        1.37        1.73
    ## 107      81.03      98.49 FALSE JOLY    Eva   23        1.68        2.08
    ## 108      78.46      98.57 FALSE JOLY    Eva   80        1.90        2.42
    ## 109      78.43      97.22 FALSE JOLY    Eva   21        1.15        1.46
    ## 110      82.39      98.32 FALSE JOLY    Eva    1        0.70        0.85
    ## 111      84.21      98.77 FALSE JOLY    Eva    0        0.00        0.00
    ## 112      87.96      96.94 FALSE JOLY    Eva   17       15.74       17.89
    ## 113      85.05      98.38 FALSE JOLY    Eva   25        2.69        3.16
    ## 114      81.29      98.58 FALSE JOLY    Eva    5        2.92        3.60
    ## 115      74.65      98.15 FALSE JOLY    Eva    1        0.70        0.94
    ## 116      79.86      97.80 FALSE JOLY    Eva    4        1.44        1.80
    ## 117      83.19      99.17 FALSE JOLY    Eva  409        1.11        1.33
    ##     Sexe.1  Nom.1 Prénom.1 Voix.1 X..Voix.Ins.1 X..Voix.Exp.1 Sexe.2
    ## 1    FALSE LE PEN   Marine    117         18.72         21.71      M
    ## 2    FALSE LE PEN   Marine     50         24.39         28.90      M
    ## 3    FALSE LE PEN   Marine     20         18.18         21.51      M
    ## 4    FALSE LE PEN   Marine     60         27.65         34.29      M
    ## 5    FALSE LE PEN   Marine     84         27.10         33.87      M
    ## 6    FALSE LE PEN   Marine     10         14.29         16.95      M
    ## 7    FALSE LE PEN   Marine     44         15.49         18.33      M
    ## 8    FALSE LE PEN   Marine   1007         26.05         32.09      M
    ## 9    FALSE LE PEN   Marine     31          9.72         10.80      M
    ## 10   FALSE LE PEN   Marine     18         10.06         11.76      M
    ## 11   FALSE LE PEN   Marine    148         14.93         17.47      M
    ## 12   FALSE LE PEN   Marine     65         22.89         27.31      M
    ## 13   FALSE LE PEN   Marine     17         21.79         25.00      M
    ## 14   FALSE LE PEN   Marine     21         23.33         29.17      M
    ## 15   FALSE LE PEN   Marine    107         19.67         24.88      M
    ## 16   FALSE LE PEN   Marine      8         15.38         21.62      M
    ## 17   FALSE LE PEN   Marine     63         10.36         12.78      M
    ## 18   FALSE LE PEN   Marine    114          9.00         10.39      M
    ## 19   FALSE LE PEN   Marine     26         24.07         27.96      M
    ## 20   FALSE LE PEN   Marine    715         13.63         16.19      M
    ## 21   FALSE LE PEN   Marine     63         24.71         29.86      M
    ## 22   FALSE LE PEN   Marine     86         21.50         27.48      M
    ## 23   FALSE LE PEN   Marine     90         17.82         20.83      M
    ## 24   FALSE LE PEN   Marine     18         11.76         15.93      M
    ## 25   FALSE LE PEN   Marine      9         12.16         14.52      M
    ## 26   FALSE LE PEN   Marine   1058         10.41         11.80      M
    ## 27   FALSE LE PEN   Marine    120         13.29         15.52      M
    ## 28   FALSE LE PEN   Marine   2524          8.11         10.69      M
    ## 29   FALSE LE PEN   Marine     73         20.51         26.35      M
    ## 30   FALSE LE PEN   Marine     21         16.41         18.92      M
    ## 31   FALSE LE PEN   Marine     30         13.70         15.63      M
    ## 32   FALSE LE PEN   Marine    135         18.39         22.54      M
    ## 33   FALSE LE PEN   Marine     61         11.64         14.25      M
    ## 34   FALSE LE PEN   Marine     45         18.00         21.13      M
    ## 35   FALSE LE PEN   Marine     40          9.30         10.78      M
    ## 36   FALSE LE PEN   Marine     58         23.29         26.61      M
    ## 37   FALSE LE PEN   Marine     38         20.32         25.00      M
    ## 38   FALSE LE PEN   Marine     40         17.47         20.00      M
    ## 39   FALSE LE PEN   Marine     32         18.60         21.62      M
    ## 40   FALSE LE PEN   Marine    610         17.03         20.12      M
    ## 41   FALSE LE PEN   Marine    221         15.81         18.63      M
    ## 42   FALSE LE PEN   Marine     33         20.37         23.91      M
    ## 43   FALSE LE PEN   Marine     20         15.04         20.20      M
    ## 44   FALSE LE PEN   Marine      8          7.84          9.09      M
    ## 45   FALSE LE PEN   Marine     39         14.89         18.48      M
    ## 46   FALSE LE PEN   Marine    261         15.13         18.38      M
    ## 47   FALSE LE PEN   Marine    497         20.30         25.03      M
    ## 48   FALSE LE PEN   Marine      5         11.63         14.71      M
    ## 49   FALSE LE PEN   Marine      8         25.81         29.63      M
    ## 50   FALSE LE PEN   Marine    161         19.80         23.89      M
    ## 51   FALSE LE PEN   Marine     74         25.17         28.24      M
    ## 52   FALSE LE PEN   Marine     12         10.34         13.04      M
    ## 53   FALSE LE PEN   Marine     22         15.49         18.64      M
    ## 54   FALSE LE PEN   Marine     62         21.38         24.60      M
    ## 55   FALSE LE PEN   Marine     99         20.84         24.32      M
    ## 56   FALSE LE PEN   Marine    189         20.95         25.40      M
    ## 57   FALSE LE PEN   Marine     78         20.58         24.45      M
    ## 58   FALSE LE PEN   Marine    202         12.04         14.09      M
    ## 59   FALSE LE PEN   Marine    783         14.89         18.59      M
    ## 60   FALSE LE PEN   Marine     10         13.51         15.38      M
    ## 61   FALSE LE PEN   Marine     32         23.02         28.32      M
    ## 62   FALSE LE PEN   Marine     52         15.48         18.77      M
    ## 63   FALSE LE PEN   Marine     10          5.10          6.58      M
    ## 64   FALSE LE PEN   Marine     17         20.99         25.76      M
    ## 65   FALSE LE PEN   Marine     21         13.55         16.15      M
    ## 66   FALSE LE PEN   Marine    124         30.17         37.46      M
    ## 67   FALSE LE PEN   Marine     72         23.76         31.17      M
    ## 68   FALSE LE PEN   Marine     34         13.93         16.83      M
    ## 69   FALSE LE PEN   Marine     35         21.34         23.81      M
    ## 70   FALSE LE PEN   Marine     30         15.96         18.99      M
    ## 71   FALSE LE PEN   Marine      6         18.75         21.43      M
    ## 72   FALSE LE PEN   Marine     13         28.26         31.71      M
    ## 73   FALSE LE PEN   Marine     14         16.87         23.73      M
    ## 74   FALSE LE PEN   Marine     14         17.95         25.45      M
    ## 75   FALSE LE PEN   Marine      7         17.07         20.59      M
    ## 76   FALSE LE PEN   Marine     19         16.96         21.84      M
    ## 77   FALSE LE PEN   Marine      8         14.55         17.02      M
    ## 78   FALSE LE PEN   Marine    516         19.81         24.69      M
    ## 79   FALSE LE PEN   Marine    402         18.47         24.86      M
    ## 80   FALSE LE PEN   Marine    239         27.73         34.34      M
    ## 81   FALSE LE PEN   Marine     45         19.40         22.84      M
    ## 82   FALSE LE PEN   Marine     37         19.17         23.13      M
    ## 83   FALSE LE PEN   Marine     40         22.99         26.49      M
    ## 84   FALSE LE PEN   Marine     74         20.05         22.29      M
    ## 85   FALSE LE PEN   Marine     47         21.27         25.00      M
    ## 86   FALSE LE PEN   Marine    100         13.02         15.67      M
    ## 87   FALSE LE PEN   Marine     19         10.73         13.38      M
    ## 88   FALSE LE PEN   Marine    336          9.26         11.29      M
    ## 89   FALSE LE PEN   Marine     40          9.98         11.30      M
    ## 90   FALSE LE PEN   Marine      1          2.22          2.56      M
    ## 91   FALSE LE PEN   Marine      8         10.53         14.55      M
    ## 92   FALSE LE PEN   Marine     93         19.79         23.37      M
    ## 93   FALSE LE PEN   Marine     36         11.32         13.38      M
    ## 94   FALSE LE PEN   Marine     35         25.93         28.00      M
    ## 95   FALSE LE PEN   Marine     19         20.00         22.09      M
    ## 96   FALSE LE PEN   Marine     46         18.93         24.60      M
    ## 97   FALSE LE PEN   Marine    127         18.04         21.67      M
    ## 98   FALSE LE PEN   Marine    131         22.13         26.90      M
    ## 99   FALSE LE PEN   Marine     69         12.55         15.86      M
    ## 100  FALSE LE PEN   Marine     25          7.33          8.53      M
    ## 101  FALSE LE PEN   Marine    170         16.72         20.56      M
    ## 102  FALSE LE PEN   Marine     54         13.04         16.07      M
    ## 103  FALSE LE PEN   Marine    118         14.66         17.23      M
    ## 104  FALSE LE PEN   Marine    117         13.46         17.73      M
    ## 105  FALSE LE PEN   Marine    131         24.53         28.54      M
    ## 106  FALSE LE PEN   Marine    312         19.46         24.47      M
    ## 107  FALSE LE PEN   Marine    193         14.14         17.45      M
    ## 108  FALSE LE PEN   Marine    517         12.25         15.61      M
    ## 109  FALSE LE PEN   Marine    247         13.49         17.20      M
    ## 110  FALSE LE PEN   Marine     31         21.83         26.50      M
    ## 111  FALSE LE PEN   Marine     30         31.58         37.50      M
    ## 112  FALSE LE PEN   Marine      5          4.63          5.26      M
    ## 113  FALSE LE PEN   Marine    136         14.62         17.19      M
    ## 114  FALSE LE PEN   Marine     43         25.15         30.94      M
    ## 115  FALSE LE PEN   Marine     33         23.24         31.13      M
    ## 116  FALSE LE PEN   Marine     47         16.91         21.17      M
    ## 117  FALSE LE PEN   Marine   1539          4.16          5.00      M
    ##       Nom.2 Prénom.2 Voix.2 X..Voix.Ins.2 X..Voix.Exp.2 Sexe.3     Nom.3
    ## 1   SARKOZY  Nicolas    148         23.68         27.46      M MÉLENCHON
    ## 2   SARKOZY  Nicolas     39         19.02         22.54      M MÉLENCHON
    ## 3   SARKOZY  Nicolas     44         40.00         47.31      M MÉLENCHON
    ## 4   SARKOZY  Nicolas     48         22.12         27.43      M MÉLENCHON
    ## 5   SARKOZY  Nicolas     44         14.19         17.74      M MÉLENCHON
    ## 6   SARKOZY  Nicolas     13         18.57         22.03      M MÉLENCHON
    ## 7   SARKOZY  Nicolas     50         17.61         20.83      M MÉLENCHON
    ## 8   SARKOZY  Nicolas    930         24.06         29.64      M MÉLENCHON
    ## 9   SARKOZY  Nicolas     60         18.81         20.91      M MÉLENCHON
    ## 10  SARKOZY  Nicolas     10          5.59          6.54      M MÉLENCHON
    ## 11  SARKOZY  Nicolas    184         18.57         21.72      M MÉLENCHON
    ## 12  SARKOZY  Nicolas    111         39.08         46.64      M MÉLENCHON
    ## 13  SARKOZY  Nicolas     43         55.13         63.24      M MÉLENCHON
    ## 14  SARKOZY  Nicolas     21         23.33         29.17      M MÉLENCHON
    ## 15  SARKOZY  Nicolas     65         11.95         15.12      M MÉLENCHON
    ## 16  SARKOZY  Nicolas      4          7.69         10.81      M MÉLENCHON
    ## 17  SARKOZY  Nicolas    203         33.39         41.18      M MÉLENCHON
    ## 18  SARKOZY  Nicolas    290         22.91         26.44      M MÉLENCHON
    ## 19  SARKOZY  Nicolas     20         18.52         21.51      M MÉLENCHON
    ## 20  SARKOZY  Nicolas    978         18.65         22.15      M MÉLENCHON
    ## 21  SARKOZY  Nicolas     54         21.18         25.59      M MÉLENCHON
    ## 22  SARKOZY  Nicolas    101         25.25         32.27      M MÉLENCHON
    ## 23  SARKOZY  Nicolas     99         19.60         22.92      M MÉLENCHON
    ## 24  SARKOZY  Nicolas     14          9.15         12.39      M MÉLENCHON
    ## 25  SARKOZY  Nicolas     19         25.68         30.65      M MÉLENCHON
    ## 26  SARKOZY  Nicolas   2246         22.09         25.04      M MÉLENCHON
    ## 27  SARKOZY  Nicolas    195         21.59         25.23      M MÉLENCHON
    ## 28  SARKOZY  Nicolas   5201         16.71         22.03      M MÉLENCHON
    ## 29  SARKOZY  Nicolas     94         26.40         33.94      M MÉLENCHON
    ## 30  SARKOZY  Nicolas     41         32.03         36.94      M MÉLENCHON
    ## 31  SARKOZY  Nicolas     66         30.14         34.38      M MÉLENCHON
    ## 32  SARKOZY  Nicolas    188         25.61         31.39      M MÉLENCHON
    ## 33  SARKOZY  Nicolas    142         27.10         33.18      M MÉLENCHON
    ## 34  SARKOZY  Nicolas     65         26.00         30.52      M MÉLENCHON
    ## 35  SARKOZY  Nicolas    103         23.95         27.76      M MÉLENCHON
    ## 36  SARKOZY  Nicolas     58         23.29         26.61      M MÉLENCHON
    ## 37  SARKOZY  Nicolas     33         17.65         21.71      M MÉLENCHON
    ## 38  SARKOZY  Nicolas     29         12.66         14.50      M MÉLENCHON
    ## 39  SARKOZY  Nicolas     45         26.16         30.41      M MÉLENCHON
    ## 40  SARKOZY  Nicolas    702         19.60         23.15      M MÉLENCHON
    ## 41  SARKOZY  Nicolas    253         18.10         21.33      M MÉLENCHON
    ## 42  SARKOZY  Nicolas     16          9.88         11.59      M MÉLENCHON
    ## 43  SARKOZY  Nicolas     27         20.30         27.27      M MÉLENCHON
    ## 44  SARKOZY  Nicolas     15         14.71         17.05      M MÉLENCHON
    ## 45  SARKOZY  Nicolas     51         19.47         24.17      M MÉLENCHON
    ## 46  SARKOZY  Nicolas    342         19.83         24.08      M MÉLENCHON
    ## 47  SARKOZY  Nicolas    589         24.06         29.66      M MÉLENCHON
    ## 48  SARKOZY  Nicolas      7         16.28         20.59      M MÉLENCHON
    ## 49  SARKOZY  Nicolas      5         16.13         18.52      M MÉLENCHON
    ## 50  SARKOZY  Nicolas    158         19.43         23.44      M MÉLENCHON
    ## 51  SARKOZY  Nicolas     61         20.75         23.28      M MÉLENCHON
    ## 52  SARKOZY  Nicolas     19         16.38         20.65      M MÉLENCHON
    ## 53  SARKOZY  Nicolas     46         32.39         38.98      M MÉLENCHON
    ## 54  SARKOZY  Nicolas     73         25.17         28.97      M MÉLENCHON
    ## 55  SARKOZY  Nicolas    101         21.26         24.82      M MÉLENCHON
    ## 56  SARKOZY  Nicolas    197         21.84         26.48      M MÉLENCHON
    ## 57  SARKOZY  Nicolas     80         21.11         25.08      M MÉLENCHON
    ## 58  SARKOZY  Nicolas    357         21.28         24.90      M MÉLENCHON
    ## 59  SARKOZY  Nicolas    600         11.41         14.24      M MÉLENCHON
    ## 60  SARKOZY  Nicolas     18         24.32         27.69      M MÉLENCHON
    ## 61  SARKOZY  Nicolas     26         18.71         23.01      M MÉLENCHON
    ## 62  SARKOZY  Nicolas     92         27.38         33.21      M MÉLENCHON
    ## 63  SARKOZY  Nicolas     29         14.80         19.08      M MÉLENCHON
    ## 64  SARKOZY  Nicolas     31         38.27         46.97      M MÉLENCHON
    ## 65  SARKOZY  Nicolas     50         32.26         38.46      M MÉLENCHON
    ## 66  SARKOZY  Nicolas     70         17.03         21.15      M MÉLENCHON
    ## 67  SARKOZY  Nicolas     69         22.77         29.87      M MÉLENCHON
    ## 68  SARKOZY  Nicolas     40         16.39         19.80      M MÉLENCHON
    ## 69  SARKOZY  Nicolas     40         24.39         27.21      M MÉLENCHON
    ## 70  SARKOZY  Nicolas     41         21.81         25.95      M MÉLENCHON
    ## 71  SARKOZY  Nicolas      7         21.88         25.00      M MÉLENCHON
    ## 72  SARKOZY  Nicolas     12         26.09         29.27      M MÉLENCHON
    ## 73  SARKOZY  Nicolas     13         15.66         22.03      M MÉLENCHON
    ## 74  SARKOZY  Nicolas     18         23.08         32.73      M MÉLENCHON
    ## 75  SARKOZY  Nicolas      8         19.51         23.53      M MÉLENCHON
    ## 76  SARKOZY  Nicolas     25         22.32         28.74      M MÉLENCHON
    ## 77  SARKOZY  Nicolas     12         21.82         25.53      M MÉLENCHON
    ## 78  SARKOZY  Nicolas    301         11.55         14.40      M MÉLENCHON
    ## 79  SARKOZY  Nicolas    209          9.60         12.93      M MÉLENCHON
    ## 80  SARKOZY  Nicolas    133         15.43         19.11      M MÉLENCHON
    ## 81  SARKOZY  Nicolas     53         22.84         26.90      M MÉLENCHON
    ## 82  SARKOZY  Nicolas     60         31.09         37.50      M MÉLENCHON
    ## 83  SARKOZY  Nicolas     45         25.86         29.80      M MÉLENCHON
    ## 84  SARKOZY  Nicolas     63         17.07         18.98      M MÉLENCHON
    ## 85  SARKOZY  Nicolas     54         24.43         28.72      M MÉLENCHON
    ## 86  SARKOZY  Nicolas    119         15.49         18.65      M MÉLENCHON
    ## 87  SARKOZY  Nicolas     45         25.42         31.69      M MÉLENCHON
    ## 88  SARKOZY  Nicolas    728         20.06         24.45      M MÉLENCHON
    ## 89  SARKOZY  Nicolas    133         33.17         37.57      M MÉLENCHON
    ## 90  SARKOZY  Nicolas      3          6.67          7.69      M MÉLENCHON
    ## 91  SARKOZY  Nicolas      3          3.95          5.45      M MÉLENCHON
    ## 92  SARKOZY  Nicolas     70         14.89         17.59      M MÉLENCHON
    ## 93  SARKOZY  Nicolas     86         27.04         31.97      M MÉLENCHON
    ## 94  SARKOZY  Nicolas     29         21.48         23.20      M MÉLENCHON
    ## 95  SARKOZY  Nicolas     27         28.42         31.40      M MÉLENCHON
    ## 96  SARKOZY  Nicolas     59         24.28         31.55      M MÉLENCHON
    ## 97  SARKOZY  Nicolas    166         23.58         28.33      M MÉLENCHON
    ## 98  SARKOZY  Nicolas     99         16.72         20.33      M MÉLENCHON
    ## 99  SARKOZY  Nicolas    111         20.18         25.52      M MÉLENCHON
    ## 100 SARKOZY  Nicolas     63         18.48         21.50      M MÉLENCHON
    ## 101 SARKOZY  Nicolas    215         21.14         26.00      M MÉLENCHON
    ## 102 SARKOZY  Nicolas     99         23.91         29.46      M MÉLENCHON
    ## 103 SARKOZY  Nicolas    130         16.15         18.98      M MÉLENCHON
    ## 104 SARKOZY  Nicolas    294         33.83         44.55      M MÉLENCHON
    ## 105 SARKOZY  Nicolas    174         32.58         37.91      M MÉLENCHON
    ## 106 SARKOZY  Nicolas    390         24.33         30.59      M MÉLENCHON
    ## 107 SARKOZY  Nicolas    349         25.57         31.56      M MÉLENCHON
    ## 108 SARKOZY  Nicolas   1212         28.72         36.61      M MÉLENCHON
    ## 109 SARKOZY  Nicolas    389         21.25         27.09      M MÉLENCHON
    ## 110 SARKOZY  Nicolas     36         25.35         30.77      M MÉLENCHON
    ## 111 SARKOZY  Nicolas     16         16.84         20.00      M MÉLENCHON
    ## 112 SARKOZY  Nicolas     20         18.52         21.05      M MÉLENCHON
    ## 113 SARKOZY  Nicolas    158         16.99         19.97      M MÉLENCHON
    ## 114 SARKOZY  Nicolas     26         15.20         18.71      M MÉLENCHON
    ## 115 SARKOZY  Nicolas     30         21.13         28.30      M MÉLENCHON
    ## 116 SARKOZY  Nicolas     56         20.14         25.23      M MÉLENCHON
    ## 117 SARKOZY  Nicolas  22360         60.43         72.64      M MÉLENCHON
    ##     Prénom.3 Voix.3 X..Voix.Ins.3 X..Voix.Exp.3 Sexe.4  Nom.4 Prénom.4
    ## 1   Jean-Luc     63         10.08         11.69      M POUTOU Philippe
    ## 2   Jean-Luc     15          7.32          8.67      M POUTOU Philippe
    ## 3   Jean-Luc      4          3.64          4.30      M POUTOU Philippe
    ## 4   Jean-Luc     11          5.07          6.29      M POUTOU Philippe
    ## 5   Jean-Luc     25          8.06         10.08      M POUTOU Philippe
    ## 6   Jean-Luc      7         10.00         11.86      M POUTOU Philippe
    ## 7   Jean-Luc     47         16.55         19.58      M POUTOU Philippe
    ## 8   Jean-Luc    281          7.27          8.95      M POUTOU Philippe
    ## 9   Jean-Luc     52         16.30         18.12      M POUTOU Philippe
    ## 10  Jean-Luc     35         19.55         22.88      M POUTOU Philippe
    ## 11  Jean-Luc    135         13.62         15.94      M POUTOU Philippe
    ## 12  Jean-Luc      8          2.82          3.36      M POUTOU Philippe
    ## 13  Jean-Luc      2          2.56          2.94      M POUTOU Philippe
    ## 14  Jean-Luc     10         11.11         13.89      M POUTOU Philippe
    ## 15  Jean-Luc     80         14.71         18.60      M POUTOU Philippe
    ## 16  Jean-Luc      6         11.54         16.22      M POUTOU Philippe
    ## 17  Jean-Luc     53          8.72         10.75      M POUTOU Philippe
    ## 18  Jean-Luc    145         11.45         13.22      M POUTOU Philippe
    ## 19  Jean-Luc     11         10.19         11.83      M POUTOU Philippe
    ## 20  Jean-Luc    453          8.64         10.26      M POUTOU Philippe
    ## 21  Jean-Luc     14          5.49          6.64      M POUTOU Philippe
    ## 22  Jean-Luc     11          2.75          3.51      M POUTOU Philippe
    ## 23  Jean-Luc     54         10.69         12.50      M POUTOU Philippe
    ## 24  Jean-Luc     24         15.69         21.24      M POUTOU Philippe
    ## 25  Jean-Luc      7          9.46         11.29      M POUTOU Philippe
    ## 26  Jean-Luc   1064         10.47         11.86      M POUTOU Philippe
    ## 27  Jean-Luc     96         10.63         12.42      M POUTOU Philippe
    ## 28  Jean-Luc   3158         10.15         13.38      M POUTOU Philippe
    ## 29  Jean-Luc     14          3.93          5.05      M POUTOU Philippe
    ## 30  Jean-Luc     12          9.38         10.81      M POUTOU Philippe
    ## 31  Jean-Luc     19          8.68          9.90      M POUTOU Philippe
    ## 32  Jean-Luc     49          6.68          8.18      M POUTOU Philippe
    ## 33  Jean-Luc     63         12.02         14.72      M POUTOU Philippe
    ## 34  Jean-Luc     15          6.00          7.04      M POUTOU Philippe
    ## 35  Jean-Luc     49         11.40         13.21      M POUTOU Philippe
    ## 36  Jean-Luc     29         11.65         13.30      M POUTOU Philippe
    ## 37  Jean-Luc     23         12.30         15.13      M POUTOU Philippe
    ## 38  Jean-Luc     32         13.97         16.00      M POUTOU Philippe
    ## 39  Jean-Luc     13          7.56          8.78      M POUTOU Philippe
    ## 40  Jean-Luc    380         10.61         12.53      M POUTOU Philippe
    ## 41  Jean-Luc    138          9.87         11.64      M POUTOU Philippe
    ## 42  Jean-Luc     18         11.11         13.04      M POUTOU Philippe
    ## 43  Jean-Luc      9          6.77          9.09      M POUTOU Philippe
    ## 44  Jean-Luc     14         13.73         15.91      M POUTOU Philippe
    ## 45  Jean-Luc     21          8.02          9.95      M POUTOU Philippe
    ## 46  Jean-Luc    173         10.03         12.18      M POUTOU Philippe
    ## 47  Jean-Luc    171          6.99          8.61      M POUTOU Philippe
    ## 48  Jean-Luc      7         16.28         20.59      M POUTOU Philippe
    ## 49  Jean-Luc      6         19.35         22.22      M POUTOU Philippe
    ## 50  Jean-Luc     68          8.36         10.09      M POUTOU Philippe
    ## 51  Jean-Luc     31         10.54         11.83      M POUTOU Philippe
    ## 52  Jean-Luc     14         12.07         15.22      M POUTOU Philippe
    ## 53  Jean-Luc     10          7.04          8.47      M POUTOU Philippe
    ## 54  Jean-Luc     21          7.24          8.33      M POUTOU Philippe
    ## 55  Jean-Luc     36          7.58          8.85      M POUTOU Philippe
    ## 56  Jean-Luc     64          7.10          8.60      M POUTOU Philippe
    ## 57  Jean-Luc     20          5.28          6.27      M POUTOU Philippe
    ## 58  Jean-Luc    165          9.83         11.51      M POUTOU Philippe
    ## 59  Jean-Luc    791         15.04         18.78      M POUTOU Philippe
    ## 60  Jean-Luc      4          5.41          6.15      M POUTOU Philippe
    ## 61  Jean-Luc     15         10.79         13.27      M POUTOU Philippe
    ## 62  Jean-Luc     38         11.31         13.72      M POUTOU Philippe
    ## 63  Jean-Luc     29         14.80         19.08      M POUTOU Philippe
    ## 64  Jean-Luc      6          7.41          9.09      M POUTOU Philippe
    ## 65  Jean-Luc     10          6.45          7.69      M POUTOU Philippe
    ## 66  Jean-Luc     17          4.14          5.14      M POUTOU Philippe
    ## 67  Jean-Luc     12          3.96          5.19      M POUTOU Philippe
    ## 68  Jean-Luc     27         11.07         13.37      M POUTOU Philippe
    ## 69  Jean-Luc     19         11.59         12.93      M POUTOU Philippe
    ## 70  Jean-Luc     23         12.23         14.56      M POUTOU Philippe
    ## 71  Jean-Luc      0          0.00          0.00      M POUTOU Philippe
    ## 72  Jean-Luc      6         13.04         14.63      M POUTOU Philippe
    ## 73  Jean-Luc      9         10.84         15.25      M POUTOU Philippe
    ## 74  Jean-Luc      8         10.26         14.55      M POUTOU Philippe
    ## 75  Jean-Luc      8         19.51         23.53      M POUTOU Philippe
    ## 76  Jean-Luc     10          8.93         11.49      M POUTOU Philippe
    ## 77  Jean-Luc      2          3.64          4.26      M POUTOU Philippe
    ## 78  Jean-Luc    614         23.57         29.38      M POUTOU Philippe
    ## 79  Jean-Luc    335         15.40         20.72      M POUTOU Philippe
    ## 80  Jean-Luc     59          6.84          8.48      M POUTOU Philippe
    ## 81  Jean-Luc     17          7.33          8.63      M POUTOU Philippe
    ## 82  Jean-Luc      8          4.15          5.00      M POUTOU Philippe
    ## 83  Jean-Luc      8          4.60          5.30      M POUTOU Philippe
    ## 84  Jean-Luc     51         13.82         15.36      M POUTOU Philippe
    ## 85  Jean-Luc     23         10.41         12.23      M POUTOU Philippe
    ## 86  Jean-Luc    104         13.54         16.30      M POUTOU Philippe
    ## 87  Jean-Luc     15          8.47         10.56      M POUTOU Philippe
    ## 88  Jean-Luc    288          7.93          9.67      M POUTOU Philippe
    ## 89  Jean-Luc     23          5.74          6.50      M POUTOU Philippe
    ## 90  Jean-Luc      5         11.11         12.82      M POUTOU Philippe
    ## 91  Jean-Luc     15         19.74         27.27      M POUTOU Philippe
    ## 92  Jean-Luc     82         17.45         20.60      M POUTOU Philippe
    ## 93  Jean-Luc     41         12.89         15.24      M POUTOU Philippe
    ## 94  Jean-Luc     12          8.89          9.60      M POUTOU Philippe
    ## 95  Jean-Luc      8          8.42          9.30      M POUTOU Philippe
    ## 96  Jean-Luc     12          4.94          6.42      M POUTOU Philippe
    ## 97  Jean-Luc     68          9.66         11.60      M POUTOU Philippe
    ## 98  Jean-Luc     62         10.47         12.73      M POUTOU Philippe
    ## 99  Jean-Luc     45          8.18         10.34      M POUTOU Philippe
    ## 100 Jean-Luc     32          9.38         10.92      M POUTOU Philippe
    ## 101 Jean-Luc    111         10.91         13.42      M POUTOU Philippe
    ## 102 Jean-Luc     51         12.32         15.18      M POUTOU Philippe
    ## 103 Jean-Luc    121         15.03         17.66      M POUTOU Philippe
    ## 104 Jean-Luc     42          4.83          6.36      M POUTOU Philippe
    ## 105 Jean-Luc     19          3.56          4.14      M POUTOU Philippe
    ## 106 Jean-Luc    102          6.36          8.00      M POUTOU Philippe
    ## 107 Jean-Luc    113          8.28         10.22      M POUTOU Philippe
    ## 108 Jean-Luc    249          5.90          7.52      M POUTOU Philippe
    ## 109 Jean-Luc    140          7.65          9.75      M POUTOU Philippe
    ## 110 Jean-Luc      8          5.63          6.84      M POUTOU Philippe
    ## 111 Jean-Luc     10         10.53         12.50      M POUTOU Philippe
    ## 112 Jean-Luc     19         17.59         20.00      M POUTOU Philippe
    ## 113 Jean-Luc    103         11.08         13.02      M POUTOU Philippe
    ## 114 Jean-Luc     18         10.53         12.95      M POUTOU Philippe
    ## 115 Jean-Luc     18         12.68         16.98      M POUTOU Philippe
    ## 116 Jean-Luc     27          9.71         12.16      M POUTOU Philippe
    ## 117 Jean-Luc    649          1.75          2.11      M POUTOU Philippe
    ##     Voix.4 X..Voix.Ins.4 X..Voix.Exp.4 Sexe.5   Nom.5 Prénom.5 Voix.5
    ## 1       11          1.76          2.04  FALSE ARTHAUD Nathalie      8
    ## 2        0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 3        0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 4        1          0.46          0.57  FALSE ARTHAUD Nathalie      2
    ## 5        3          0.97          1.21  FALSE ARTHAUD Nathalie      7
    ## 6        1          1.43          1.69  FALSE ARTHAUD Nathalie      0
    ## 7        3          1.06          1.25  FALSE ARTHAUD Nathalie      0
    ## 8       24          0.62          0.76  FALSE ARTHAUD Nathalie     11
    ## 9        8          2.51          2.79  FALSE ARTHAUD Nathalie      2
    ## 10       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 11       9          0.91          1.06  FALSE ARTHAUD Nathalie      5
    ## 12       2          0.70          0.84  FALSE ARTHAUD Nathalie      0
    ## 13       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 14       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 15       7          1.29          1.63  FALSE ARTHAUD Nathalie      1
    ## 16       1          1.92          2.70  FALSE ARTHAUD Nathalie      1
    ## 17       5          0.82          1.01  FALSE ARTHAUD Nathalie      2
    ## 18      23          1.82          2.10  FALSE ARTHAUD Nathalie     11
    ## 19       5          4.63          5.38  FALSE ARTHAUD Nathalie      1
    ## 20      64          1.22          1.45  FALSE ARTHAUD Nathalie     21
    ## 21       4          1.57          1.90  FALSE ARTHAUD Nathalie      0
    ## 22       5          1.25          1.60  FALSE ARTHAUD Nathalie      0
    ## 23       8          1.58          1.85  FALSE ARTHAUD Nathalie      2
    ## 24       4          2.61          3.54  FALSE ARTHAUD Nathalie      0
    ## 25       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 26     115          1.13          1.28  FALSE ARTHAUD Nathalie     34
    ## 27       8          0.89          1.03  FALSE ARTHAUD Nathalie      4
    ## 28     268          0.86          1.14  FALSE ARTHAUD Nathalie    132
    ## 29       3          0.84          1.08  FALSE ARTHAUD Nathalie      4
    ## 30       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 31       5          2.28          2.60  FALSE ARTHAUD Nathalie      0
    ## 32       5          0.68          0.83  FALSE ARTHAUD Nathalie      2
    ## 33       2          0.38          0.47  FALSE ARTHAUD Nathalie      3
    ## 34       6          2.40          2.82  FALSE ARTHAUD Nathalie      0
    ## 35       4          0.93          1.08  FALSE ARTHAUD Nathalie      3
    ## 36       6          2.41          2.75  FALSE ARTHAUD Nathalie      0
    ## 37       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 38       3          1.31          1.50  FALSE ARTHAUD Nathalie      3
    ## 39       0          0.00          0.00  FALSE ARTHAUD Nathalie      3
    ## 40      36          1.01          1.19  FALSE ARTHAUD Nathalie     15
    ## 41      17          1.22          1.43  FALSE ARTHAUD Nathalie      8
    ## 42       3          1.85          2.17  FALSE ARTHAUD Nathalie      0
    ## 43       2          1.50          2.02  FALSE ARTHAUD Nathalie      0
    ## 44       3          2.94          3.41  FALSE ARTHAUD Nathalie      0
    ## 45       6          2.29          2.84  FALSE ARTHAUD Nathalie      0
    ## 46      15          0.87          1.06  FALSE ARTHAUD Nathalie      6
    ## 47      16          0.65          0.81  FALSE ARTHAUD Nathalie     10
    ## 48       1          2.33          2.94  FALSE ARTHAUD Nathalie      1
    ## 49       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 50      12          1.48          1.78  FALSE ARTHAUD Nathalie      4
    ## 51       4          1.36          1.53  FALSE ARTHAUD Nathalie      2
    ## 52       2          1.72          2.17  FALSE ARTHAUD Nathalie      1
    ## 53       1          0.70          0.85  FALSE ARTHAUD Nathalie      0
    ## 54       2          0.69          0.79  FALSE ARTHAUD Nathalie      0
    ## 55       6          1.26          1.47  FALSE ARTHAUD Nathalie      1
    ## 56       5          0.55          0.67  FALSE ARTHAUD Nathalie      3
    ## 57       2          0.53          0.63  FALSE ARTHAUD Nathalie      1
    ## 58      22          1.31          1.53  FALSE ARTHAUD Nathalie     19
    ## 59      70          1.33          1.66  FALSE ARTHAUD Nathalie     27
    ## 60       4          5.41          6.15  FALSE ARTHAUD Nathalie      0
    ## 61       3          2.16          2.65  FALSE ARTHAUD Nathalie      2
    ## 62       4          1.19          1.44  FALSE ARTHAUD Nathalie      0
    ## 63       3          1.53          1.97  FALSE ARTHAUD Nathalie      2
    ## 64       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 65       5          3.23          3.85  FALSE ARTHAUD Nathalie      1
    ## 66       1          0.24          0.30  FALSE ARTHAUD Nathalie      2
    ## 67       3          0.99          1.30  FALSE ARTHAUD Nathalie      3
    ## 68       4          1.64          1.98  FALSE ARTHAUD Nathalie      5
    ## 69       6          3.66          4.08  FALSE ARTHAUD Nathalie      2
    ## 70       0          0.00          0.00  FALSE ARTHAUD Nathalie      1
    ## 71       2          6.25          7.14  FALSE ARTHAUD Nathalie      0
    ## 72       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 73       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 74       0          0.00          0.00  FALSE ARTHAUD Nathalie      1
    ## 75       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 76       3          2.68          3.45  FALSE ARTHAUD Nathalie      0
    ## 77       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 78      19          0.73          0.91  FALSE ARTHAUD Nathalie      6
    ## 79      17          0.78          1.05  FALSE ARTHAUD Nathalie      9
    ## 80      16          1.86          2.30  FALSE ARTHAUD Nathalie     11
    ## 81       4          1.72          2.03  FALSE ARTHAUD Nathalie      0
    ## 82       1          0.52          0.63  FALSE ARTHAUD Nathalie      0
    ## 83       3          1.72          1.99  FALSE ARTHAUD Nathalie      3
    ## 84       9          2.44          2.71  FALSE ARTHAUD Nathalie      1
    ## 85       1          0.45          0.53  FALSE ARTHAUD Nathalie      1
    ## 86      11          1.43          1.72  FALSE ARTHAUD Nathalie      6
    ## 87       0          0.00          0.00  FALSE ARTHAUD Nathalie      2
    ## 88      28          0.77          0.94  FALSE ARTHAUD Nathalie      7
    ## 89       2          0.50          0.56  FALSE ARTHAUD Nathalie      0
    ## 90       0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 91       0          0.00          0.00  FALSE ARTHAUD Nathalie      1
    ## 92       4          0.85          1.01  FALSE ARTHAUD Nathalie      1
    ## 93       2          0.63          0.74  FALSE ARTHAUD Nathalie      0
    ## 94       0          0.00          0.00  FALSE ARTHAUD Nathalie      2
    ## 95       0          0.00          0.00  FALSE ARTHAUD Nathalie      3
    ## 96       2          0.82          1.07  FALSE ARTHAUD Nathalie      1
    ## 97       7          0.99          1.19  FALSE ARTHAUD Nathalie      1
    ## 98       5          0.84          1.03  FALSE ARTHAUD Nathalie      2
    ## 99       4          0.73          0.92  FALSE ARTHAUD Nathalie      3
    ## 100      9          2.64          3.07  FALSE ARTHAUD Nathalie      5
    ## 101     10          0.98          1.21  FALSE ARTHAUD Nathalie      6
    ## 102      7          1.69          2.08  FALSE ARTHAUD Nathalie      2
    ## 103     13          1.61          1.90  FALSE ARTHAUD Nathalie      4
    ## 104     10          1.15          1.52  FALSE ARTHAUD Nathalie      3
    ## 105      3          0.56          0.65  FALSE ARTHAUD Nathalie      6
    ## 106     12          0.75          0.94  FALSE ARTHAUD Nathalie      5
    ## 107     10          0.73          0.90  FALSE ARTHAUD Nathalie      5
    ## 108     28          0.66          0.85  FALSE ARTHAUD Nathalie      6
    ## 109     38          2.08          2.65  FALSE ARTHAUD Nathalie      6
    ## 110      0          0.00          0.00  FALSE ARTHAUD Nathalie      0
    ## 111      1          1.05          1.25  FALSE ARTHAUD Nathalie      2
    ## 112      1          0.93          1.05  FALSE ARTHAUD Nathalie      0
    ## 113     11          1.18          1.39  FALSE ARTHAUD Nathalie      8
    ## 114      1          0.58          0.72  FALSE ARTHAUD Nathalie      1
    ## 115      0          0.00          0.00  FALSE ARTHAUD Nathalie      2
    ## 116      4          1.44          1.80  FALSE ARTHAUD Nathalie      0
    ## 117     67          0.18          0.22  FALSE ARTHAUD Nathalie     22
    ##     X..Voix.Ins.5 X..Voix.Exp.5 Sexe.6     Nom.6 Prénom.6 Voix.6
    ## 1            1.28          1.48      M CHEMINADE  Jacques      1
    ## 2            0.00          0.00      M CHEMINADE  Jacques      0
    ## 3            0.00          0.00      M CHEMINADE  Jacques      0
    ## 4            0.92          1.14      M CHEMINADE  Jacques      0
    ## 5            2.26          2.82      M CHEMINADE  Jacques      1
    ## 6            0.00          0.00      M CHEMINADE  Jacques      0
    ## 7            0.00          0.00      M CHEMINADE  Jacques      0
    ## 8            0.28          0.35      M CHEMINADE  Jacques      5
    ## 9            0.63          0.70      M CHEMINADE  Jacques      0
    ## 10           0.00          0.00      M CHEMINADE  Jacques      0
    ## 11           0.50          0.59      M CHEMINADE  Jacques      2
    ## 12           0.00          0.00      M CHEMINADE  Jacques      1
    ## 13           0.00          0.00      M CHEMINADE  Jacques      0
    ## 14           0.00          0.00      M CHEMINADE  Jacques      0
    ## 15           0.18          0.23      M CHEMINADE  Jacques      0
    ## 16           1.92          2.70      M CHEMINADE  Jacques      0
    ## 17           0.33          0.41      M CHEMINADE  Jacques      0
    ## 18           0.87          1.00      M CHEMINADE  Jacques      7
    ## 19           0.93          1.08      M CHEMINADE  Jacques      1
    ## 20           0.40          0.48      M CHEMINADE  Jacques      7
    ## 21           0.00          0.00      M CHEMINADE  Jacques      0
    ## 22           0.00          0.00      M CHEMINADE  Jacques      1
    ## 23           0.40          0.46      M CHEMINADE  Jacques      0
    ## 24           0.00          0.00      M CHEMINADE  Jacques      1
    ## 25           0.00          0.00      M CHEMINADE  Jacques      0
    ## 26           0.33          0.38      M CHEMINADE  Jacques     21
    ## 27           0.44          0.52      M CHEMINADE  Jacques      1
    ## 28           0.42          0.56      M CHEMINADE  Jacques     71
    ## 29           1.12          1.44      M CHEMINADE  Jacques      0
    ## 30           0.00          0.00      M CHEMINADE  Jacques      0
    ## 31           0.00          0.00      M CHEMINADE  Jacques      1
    ## 32           0.27          0.33      M CHEMINADE  Jacques      2
    ## 33           0.57          0.70      M CHEMINADE  Jacques      1
    ## 34           0.00          0.00      M CHEMINADE  Jacques      0
    ## 35           0.70          0.81      M CHEMINADE  Jacques      2
    ## 36           0.00          0.00      M CHEMINADE  Jacques      0
    ## 37           0.00          0.00      M CHEMINADE  Jacques      3
    ## 38           1.31          1.50      M CHEMINADE  Jacques      1
    ## 39           1.74          2.03      M CHEMINADE  Jacques      0
    ## 40           0.42          0.49      M CHEMINADE  Jacques      5
    ## 41           0.57          0.67      M CHEMINADE  Jacques      2
    ## 42           0.00          0.00      M CHEMINADE  Jacques      0
    ## 43           0.00          0.00      M CHEMINADE  Jacques      0
    ## 44           0.00          0.00      M CHEMINADE  Jacques      0
    ## 45           0.00          0.00      M CHEMINADE  Jacques      0
    ## 46           0.35          0.42      M CHEMINADE  Jacques      2
    ## 47           0.41          0.50      M CHEMINADE  Jacques      5
    ## 48           2.33          2.94      M CHEMINADE  Jacques      1
    ## 49           0.00          0.00      M CHEMINADE  Jacques      0
    ## 50           0.49          0.59      M CHEMINADE  Jacques      3
    ## 51           0.68          0.76      M CHEMINADE  Jacques      0
    ## 52           0.86          1.09      M CHEMINADE  Jacques      1
    ## 53           0.00          0.00      M CHEMINADE  Jacques      0
    ## 54           0.00          0.00      M CHEMINADE  Jacques      0
    ## 55           0.21          0.25      M CHEMINADE  Jacques      1
    ## 56           0.33          0.40      M CHEMINADE  Jacques      0
    ## 57           0.26          0.31      M CHEMINADE  Jacques      0
    ## 58           1.13          1.32      M CHEMINADE  Jacques      2
    ## 59           0.51          0.64      M CHEMINADE  Jacques      3
    ## 60           0.00          0.00      M CHEMINADE  Jacques      0
    ## 61           1.44          1.77      M CHEMINADE  Jacques      0
    ## 62           0.00          0.00      M CHEMINADE  Jacques      1
    ## 63           1.02          1.32      M CHEMINADE  Jacques      0
    ## 64           0.00          0.00      M CHEMINADE  Jacques      0
    ## 65           0.65          0.77      M CHEMINADE  Jacques      0
    ## 66           0.49          0.60      M CHEMINADE  Jacques      0
    ## 67           0.99          1.30      M CHEMINADE  Jacques      0
    ## 68           2.05          2.48      M CHEMINADE  Jacques      0
    ## 69           1.22          1.36      M CHEMINADE  Jacques      0
    ## 70           0.53          0.63      M CHEMINADE  Jacques      1
    ## 71           0.00          0.00      M CHEMINADE  Jacques      0
    ## 72           0.00          0.00      M CHEMINADE  Jacques      0
    ## 73           0.00          0.00      M CHEMINADE  Jacques      0
    ## 74           1.28          1.82      M CHEMINADE  Jacques      0
    ## 75           0.00          0.00      M CHEMINADE  Jacques      0
    ## 76           0.00          0.00      M CHEMINADE  Jacques      0
    ## 77           0.00          0.00      M CHEMINADE  Jacques      0
    ## 78           0.23          0.29      M CHEMINADE  Jacques      0
    ## 79           0.41          0.56      M CHEMINADE  Jacques      2
    ## 80           1.28          1.58      M CHEMINADE  Jacques      1
    ## 81           0.00          0.00      M CHEMINADE  Jacques      0
    ## 82           0.00          0.00      M CHEMINADE  Jacques      0
    ## 83           1.72          1.99      M CHEMINADE  Jacques      0
    ## 84           0.27          0.30      M CHEMINADE  Jacques      0
    ## 85           0.45          0.53      M CHEMINADE  Jacques      0
    ## 86           0.78          0.94      M CHEMINADE  Jacques      1
    ## 87           1.13          1.41      M CHEMINADE  Jacques      0
    ## 88           0.19          0.24      M CHEMINADE  Jacques      7
    ## 89           0.00          0.00      M CHEMINADE  Jacques      0
    ## 90           0.00          0.00      M CHEMINADE  Jacques      0
    ## 91           1.32          1.82      M CHEMINADE  Jacques      1
    ## 92           0.21          0.25      M CHEMINADE  Jacques      2
    ## 93           0.00          0.00      M CHEMINADE  Jacques      0
    ## 94           1.48          1.60      M CHEMINADE  Jacques      2
    ## 95           3.16          3.49      M CHEMINADE  Jacques      0
    ## 96           0.41          0.53      M CHEMINADE  Jacques      1
    ## 97           0.14          0.17      M CHEMINADE  Jacques      0
    ## 98           0.34          0.41      M CHEMINADE  Jacques      2
    ## 99           0.55          0.69      M CHEMINADE  Jacques      2
    ## 100          1.47          1.71      M CHEMINADE  Jacques      5
    ## 101          0.59          0.73      M CHEMINADE  Jacques      5
    ## 102          0.48          0.60      M CHEMINADE  Jacques      0
    ## 103          0.50          0.58      M CHEMINADE  Jacques      2
    ## 104          0.35          0.45      M CHEMINADE  Jacques      1
    ## 105          1.12          1.31      M CHEMINADE  Jacques      1
    ## 106          0.31          0.39      M CHEMINADE  Jacques      3
    ## 107          0.37          0.45      M CHEMINADE  Jacques      3
    ## 108          0.14          0.18      M CHEMINADE  Jacques      6
    ## 109          0.33          0.42      M CHEMINADE  Jacques      3
    ## 110          0.00          0.00      M CHEMINADE  Jacques      0
    ## 111          2.11          2.50      M CHEMINADE  Jacques      0
    ## 112          0.00          0.00      M CHEMINADE  Jacques      0
    ## 113          0.86          1.01      M CHEMINADE  Jacques      1
    ## 114          0.58          0.72      M CHEMINADE  Jacques      0
    ## 115          1.41          1.89      M CHEMINADE  Jacques      0
    ## 116          0.00          0.00      M CHEMINADE  Jacques      0
    ## 117          0.06          0.07      M CHEMINADE  Jacques     57
    ##     X..Voix.Ins.6 X..Voix.Exp.6 Sexe.7  Nom.7 Prénom.7 Voix.7
    ## 1            0.16          0.19      M BAYROU François     57
    ## 2            0.00          0.00      M BAYROU François      7
    ## 3            0.00          0.00      M BAYROU François      8
    ## 4            0.00          0.00      M BAYROU François     14
    ## 5            0.32          0.40      M BAYROU François     24
    ## 6            0.00          0.00      M BAYROU François      8
    ## 7            0.00          0.00      M BAYROU François     16
    ## 8            0.13          0.16      M BAYROU François    220
    ## 9            0.00          0.00      M BAYROU François     43
    ## 10           0.00          0.00      M BAYROU François      8
    ## 11           0.20          0.24      M BAYROU François     54
    ## 12           0.35          0.42      M BAYROU François     16
    ## 13           0.00          0.00      M BAYROU François      1
    ## 14           0.00          0.00      M BAYROU François      5
    ## 15           0.00          0.00      M BAYROU François     27
    ## 16           0.00          0.00      M BAYROU François      0
    ## 17           0.00          0.00      M BAYROU François     44
    ## 18           0.55          0.64      M BAYROU François    114
    ## 19           0.93          1.08      M BAYROU François      8
    ## 20           0.13          0.16      M BAYROU François    478
    ## 21           0.00          0.00      M BAYROU François     22
    ## 22           0.25          0.32      M BAYROU François     29
    ## 23           0.00          0.00      M BAYROU François     31
    ## 24           0.65          0.88      M BAYROU François      3
    ## 25           0.00          0.00      M BAYROU François      9
    ## 26           0.21          0.23      M BAYROU François   1004
    ## 27           0.11          0.13      M BAYROU François     57
    ## 28           0.23          0.30      M BAYROU François   2606
    ## 29           0.00          0.00      M BAYROU François     29
    ## 30           0.00          0.00      M BAYROU François     10
    ## 31           0.46          0.52      M BAYROU François     22
    ## 32           0.27          0.33      M BAYROU François     53
    ## 33           0.19          0.23      M BAYROU François     40
    ## 34           0.00          0.00      M BAYROU François     29
    ## 35           0.47          0.54      M BAYROU François     36
    ## 36           0.00          0.00      M BAYROU François     12
    ## 37           1.60          1.97      M BAYROU François     14
    ## 38           0.44          0.50      M BAYROU François     12
    ## 39           0.00          0.00      M BAYROU François     13
    ## 40           0.14          0.16      M BAYROU François    212
    ## 41           0.14          0.17      M BAYROU François     79
    ## 42           0.00          0.00      M BAYROU François     12
    ## 43           0.00          0.00      M BAYROU François     17
    ## 44           0.00          0.00      M BAYROU François      9
    ## 45           0.00          0.00      M BAYROU François     19
    ## 46           0.12          0.14      M BAYROU François    160
    ## 47           0.20          0.25      M BAYROU François    180
    ## 48           2.33          2.94      M BAYROU François      1
    ## 49           0.00          0.00      M BAYROU François      3
    ## 50           0.37          0.45      M BAYROU François     54
    ## 51           0.00          0.00      M BAYROU François     24
    ## 52           0.86          1.09      M BAYROU François      7
    ## 53           0.00          0.00      M BAYROU François     18
    ## 54           0.00          0.00      M BAYROU François     28
    ## 55           0.21          0.25      M BAYROU François     57
    ## 56           0.00          0.00      M BAYROU François     94
    ## 57           0.00          0.00      M BAYROU François     39
    ## 58           0.12          0.14      M BAYROU François    205
    ## 59           0.06          0.07      M BAYROU François    263
    ## 60           0.00          0.00      M BAYROU François      7
    ## 61           0.00          0.00      M BAYROU François      9
    ## 62           0.30          0.36      M BAYROU François     29
    ## 63           0.00          0.00      M BAYROU François     20
    ## 64           0.00          0.00      M BAYROU François      6
    ## 65           0.00          0.00      M BAYROU François     24
    ## 66           0.00          0.00      M BAYROU François     24
    ## 67           0.00          0.00      M BAYROU François     15
    ## 68           0.00          0.00      M BAYROU François     28
    ## 69           0.00          0.00      M BAYROU François     11
    ## 70           0.53          0.63      M BAYROU François     13
    ## 71           0.00          0.00      M BAYROU François      6
    ## 72           0.00          0.00      M BAYROU François      3
    ## 73           0.00          0.00      M BAYROU François      2
    ## 74           0.00          0.00      M BAYROU François      2
    ## 75           0.00          0.00      M BAYROU François      1
    ## 76           0.00          0.00      M BAYROU François      4
    ## 77           0.00          0.00      M BAYROU François      2
    ## 78           0.00          0.00      M BAYROU François     88
    ## 79           0.09          0.12      M BAYROU François     44
    ## 80           0.12          0.14      M BAYROU François     31
    ## 81           0.00          0.00      M BAYROU François     19
    ## 82           0.00          0.00      M BAYROU François     11
    ## 83           0.00          0.00      M BAYROU François      9
    ## 84           0.00          0.00      M BAYROU François     21
    ## 85           0.00          0.00      M BAYROU François     16
    ## 86           0.13          0.16      M BAYROU François     51
    ## 87           0.00          0.00      M BAYROU François     12
    ## 88           0.19          0.24      M BAYROU François    583
    ## 89           0.00          0.00      M BAYROU François     51
    ## 90           0.00          0.00      M BAYROU François     13
    ## 91           1.32          1.82      M BAYROU François      7
    ## 92           0.43          0.50      M BAYROU François     16
    ## 93           0.00          0.00      M BAYROU François     28
    ## 94           1.48          1.60      M BAYROU François      4
    ## 95           0.00          0.00      M BAYROU François      5
    ## 96           0.41          0.53      M BAYROU François     21
    ## 97           0.00          0.00      M BAYROU François     66
    ## 98           0.34          0.41      M BAYROU François     31
    ## 99           0.36          0.46      M BAYROU François     27
    ## 100          1.47          1.71      M BAYROU François     47
    ## 101          0.49          0.60      M BAYROU François     64
    ## 102          0.00          0.00      M BAYROU François     19
    ## 103          0.25          0.29      M BAYROU François     67
    ## 104          0.12          0.15      M BAYROU François     60
    ## 105          0.19          0.22      M BAYROU François     27
    ## 106          0.19          0.24      M BAYROU François    114
    ## 107          0.22          0.27      M BAYROU François    107
    ## 108          0.14          0.18      M BAYROU François    360
    ## 109          0.16          0.21      M BAYROU François    147
    ## 110          0.00          0.00      M BAYROU François     16
    ## 111          0.00          0.00      M BAYROU François      9
    ## 112          0.00          0.00      M BAYROU François     19
    ## 113          0.11          0.13      M BAYROU François     74
    ## 114          0.00          0.00      M BAYROU François     10
    ## 115          0.00          0.00      M BAYROU François      6
    ## 116          0.00          0.00      M BAYROU François     28
    ## 117          0.15          0.19      M BAYROU François   2292
    ##     X..Voix.Ins.7 X..Voix.Exp.7 Sexe.8         Nom.8 Prénom.8 Voix.8
    ## 1            9.12         10.58      M DUPONT-AIGNAN  Nicolas      6
    ## 2            3.41          4.05      M DUPONT-AIGNAN  Nicolas      1
    ## 3            7.27          8.60      M DUPONT-AIGNAN  Nicolas      0
    ## 4            6.45          8.00      M DUPONT-AIGNAN  Nicolas      8
    ## 5            7.74          9.68      M DUPONT-AIGNAN  Nicolas      9
    ## 6           11.43         13.56      M DUPONT-AIGNAN  Nicolas      1
    ## 7            5.63          6.67      M DUPONT-AIGNAN  Nicolas      3
    ## 8            5.69          7.01      M DUPONT-AIGNAN  Nicolas     57
    ## 9           13.48         14.98      M DUPONT-AIGNAN  Nicolas      5
    ## 10           4.47          5.23      M DUPONT-AIGNAN  Nicolas      0
    ## 11           5.45          6.38      M DUPONT-AIGNAN  Nicolas     11
    ## 12           5.63          6.72      M DUPONT-AIGNAN  Nicolas      2
    ## 13           1.28          1.47      M DUPONT-AIGNAN  Nicolas      1
    ## 14           5.56          6.94      M DUPONT-AIGNAN  Nicolas      2
    ## 15           4.96          6.28      M DUPONT-AIGNAN  Nicolas      6
    ## 16           0.00          0.00      M DUPONT-AIGNAN  Nicolas      1
    ## 17           7.24          8.92      M DUPONT-AIGNAN  Nicolas     10
    ## 18           9.00         10.39      M DUPONT-AIGNAN  Nicolas     18
    ## 19           7.41          8.60      M DUPONT-AIGNAN  Nicolas      2
    ## 20           9.11         10.83      M DUPONT-AIGNAN  Nicolas     77
    ## 21           8.63         10.43      M DUPONT-AIGNAN  Nicolas      3
    ## 22           7.25          9.27      M DUPONT-AIGNAN  Nicolas      8
    ## 23           6.14          7.18      M DUPONT-AIGNAN  Nicolas     21
    ## 24           1.96          2.65      M DUPONT-AIGNAN  Nicolas      0
    ## 25          12.16         14.52      M DUPONT-AIGNAN  Nicolas      4
    ## 26           9.88         11.19      M DUPONT-AIGNAN  Nicolas    153
    ## 27           6.31          7.37      M DUPONT-AIGNAN  Nicolas     18
    ## 28           8.37         11.04      M DUPONT-AIGNAN  Nicolas    320
    ## 29           8.15         10.47      M DUPONT-AIGNAN  Nicolas      7
    ## 30           7.81          9.01      M DUPONT-AIGNAN  Nicolas      2
    ## 31          10.05         11.46      M DUPONT-AIGNAN  Nicolas      3
    ## 32           7.22          8.85      M DUPONT-AIGNAN  Nicolas     23
    ## 33           7.63          9.35      M DUPONT-AIGNAN  Nicolas      7
    ## 34          11.60         13.62      M DUPONT-AIGNAN  Nicolas      4
    ## 35           8.37          9.70      M DUPONT-AIGNAN  Nicolas      5
    ## 36           4.82          5.50      M DUPONT-AIGNAN  Nicolas      4
    ## 37           7.49          9.21      M DUPONT-AIGNAN  Nicolas      3
    ## 38           5.24          6.00      M DUPONT-AIGNAN  Nicolas      3
    ## 39           7.56          8.78      M DUPONT-AIGNAN  Nicolas      3
    ## 40           5.92          6.99      M DUPONT-AIGNAN  Nicolas     40
    ## 41           5.65          6.66      M DUPONT-AIGNAN  Nicolas     23
    ## 42           7.41          8.70      M DUPONT-AIGNAN  Nicolas      4
    ## 43          12.78         17.17      M DUPONT-AIGNAN  Nicolas      3
    ## 44           8.82         10.23      M DUPONT-AIGNAN  Nicolas      1
    ## 45           7.25          9.00      M DUPONT-AIGNAN  Nicolas      5
    ## 46           9.28         11.27      M DUPONT-AIGNAN  Nicolas     33
    ## 47           7.35          9.06      M DUPONT-AIGNAN  Nicolas     41
    ## 48           2.33          2.94      M DUPONT-AIGNAN  Nicolas      1
    ## 49           9.68         11.11      M DUPONT-AIGNAN  Nicolas      0
    ## 50           6.64          8.01      M DUPONT-AIGNAN  Nicolas     15
    ## 51           8.16          9.16      M DUPONT-AIGNAN  Nicolas      6
    ## 52           6.03          7.61      M DUPONT-AIGNAN  Nicolas      1
    ## 53          12.68         15.25      M DUPONT-AIGNAN  Nicolas      2
    ## 54           9.66         11.11      M DUPONT-AIGNAN  Nicolas      6
    ## 55          12.00         14.00      M DUPONT-AIGNAN  Nicolas     13
    ## 56          10.42         12.63      M DUPONT-AIGNAN  Nicolas     13
    ## 57          10.29         12.23      M DUPONT-AIGNAN  Nicolas      6
    ## 58          12.22         14.30      M DUPONT-AIGNAN  Nicolas     39
    ## 59           5.00          6.24      M DUPONT-AIGNAN  Nicolas     80
    ## 60           9.46         10.77      M DUPONT-AIGNAN  Nicolas      2
    ## 61           6.47          7.96      M DUPONT-AIGNAN  Nicolas      3
    ## 62           8.63         10.47      M DUPONT-AIGNAN  Nicolas      4
    ## 63          10.20         13.16      M DUPONT-AIGNAN  Nicolas      4
    ## 64           7.41          9.09      M DUPONT-AIGNAN  Nicolas      4
    ## 65          15.48         18.46      M DUPONT-AIGNAN  Nicolas      1
    ## 66           5.84          7.25      M DUPONT-AIGNAN  Nicolas     15
    ## 67           4.95          6.49      M DUPONT-AIGNAN  Nicolas      7
    ## 68          11.48         13.86      M DUPONT-AIGNAN  Nicolas      2
    ## 69           6.71          7.48      M DUPONT-AIGNAN  Nicolas      6
    ## 70           6.91          8.23      M DUPONT-AIGNAN  Nicolas      3
    ## 71          18.75         21.43      M DUPONT-AIGNAN  Nicolas      1
    ## 72           6.52          7.32      M DUPONT-AIGNAN  Nicolas      2
    ## 73           2.41          3.39      M DUPONT-AIGNAN  Nicolas      0
    ## 74           2.56          3.64      M DUPONT-AIGNAN  Nicolas      0
    ## 75           2.44          2.94      M DUPONT-AIGNAN  Nicolas      0
    ## 76           3.57          4.60      M DUPONT-AIGNAN  Nicolas      3
    ## 77           3.64          4.26      M DUPONT-AIGNAN  Nicolas      1
    ## 78           3.38          4.21      M DUPONT-AIGNAN  Nicolas     39
    ## 79           2.02          2.72      M DUPONT-AIGNAN  Nicolas     22
    ## 80           3.60          4.45      M DUPONT-AIGNAN  Nicolas     16
    ## 81           8.19          9.64      M DUPONT-AIGNAN  Nicolas      5
    ## 82           5.70          6.88      M DUPONT-AIGNAN  Nicolas      3
    ## 83           5.17          5.96      M DUPONT-AIGNAN  Nicolas      6
    ## 84           5.69          6.33      M DUPONT-AIGNAN  Nicolas      4
    ## 85           7.24          8.51      M DUPONT-AIGNAN  Nicolas      3
    ## 86           6.64          7.99      M DUPONT-AIGNAN  Nicolas      6
    ## 87           6.78          8.45      M DUPONT-AIGNAN  Nicolas      1
    ## 88          16.06         19.58      M DUPONT-AIGNAN  Nicolas     37
    ## 89          12.72         14.41      M DUPONT-AIGNAN  Nicolas      7
    ## 90          28.89         33.33      M DUPONT-AIGNAN  Nicolas      2
    ## 91           9.21         12.73      M DUPONT-AIGNAN  Nicolas      0
    ## 92           3.40          4.02      M DUPONT-AIGNAN  Nicolas      5
    ## 93           8.81         10.41      M DUPONT-AIGNAN  Nicolas     11
    ## 94           2.96          3.20      M DUPONT-AIGNAN  Nicolas      3
    ## 95           5.26          5.81      M DUPONT-AIGNAN  Nicolas      1
    ## 96           8.64         11.23      M DUPONT-AIGNAN  Nicolas      2
    ## 97           9.38         11.26      M DUPONT-AIGNAN  Nicolas     17
    ## 98           5.24          6.37      M DUPONT-AIGNAN  Nicolas      7
    ## 99           4.91          6.21      M DUPONT-AIGNAN  Nicolas      8
    ## 100         13.78         16.04      M DUPONT-AIGNAN  Nicolas      5
    ## 101          6.29          7.74      M DUPONT-AIGNAN  Nicolas     16
    ## 102          4.59          5.65      M DUPONT-AIGNAN  Nicolas      6
    ## 103          8.32          9.78      M DUPONT-AIGNAN  Nicolas     18
    ## 104          6.90          9.09      M DUPONT-AIGNAN  Nicolas      6
    ## 105          5.06          5.88      M DUPONT-AIGNAN  Nicolas      7
    ## 106          7.11          8.94      M DUPONT-AIGNAN  Nicolas     29
    ## 107          7.84          9.67      M DUPONT-AIGNAN  Nicolas     15
    ## 108          8.53         10.87      M DUPONT-AIGNAN  Nicolas     83
    ## 109          8.03         10.24      M DUPONT-AIGNAN  Nicolas     45
    ## 110         11.27         13.68      M DUPONT-AIGNAN  Nicolas      0
    ## 111          9.47         11.25      M DUPONT-AIGNAN  Nicolas      2
    ## 112         17.59         20.00      M DUPONT-AIGNAN  Nicolas      1
    ## 113          7.96          9.36      M DUPONT-AIGNAN  Nicolas     21
    ## 114          5.85          7.19      M DUPONT-AIGNAN  Nicolas      7
    ## 115          4.23          5.66      M DUPONT-AIGNAN  Nicolas      1
    ## 116         10.07         12.61      M DUPONT-AIGNAN  Nicolas      4
    ## 117          6.19          7.45      M DUPONT-AIGNAN  Nicolas    255
    ##     X..Voix.Ins.8 X..Voix.Exp.8 Sexe.9    Nom.9 Prénom.9 Voix.9
    ## 1            0.96          1.11      M HOLLANDE François    117
    ## 2            0.49          0.58      M HOLLANDE François     61
    ## 3            0.00          0.00      M HOLLANDE François     16
    ## 4            3.69          4.57      M HOLLANDE François     26
    ## 5            2.90          3.63      M HOLLANDE François     46
    ## 6            1.43          1.69      M HOLLANDE François     17
    ## 7            1.06          1.25      M HOLLANDE François     74
    ## 8            1.47          1.82      M HOLLANDE François    533
    ## 9            1.57          1.74      M HOLLANDE François     71
    ## 10           0.00          0.00      M HOLLANDE François     73
    ## 11           1.11          1.30      M HOLLANDE François    286
    ## 12           0.70          0.84      M HOLLANDE François     31
    ## 13           1.28          1.47      M HOLLANDE François      4
    ## 14           2.22          2.78      M HOLLANDE François     12
    ## 15           1.10          1.40      M HOLLANDE François    126
    ## 16           1.92          2.70      M HOLLANDE François     15
    ## 17           1.64          2.03      M HOLLANDE François    108
    ## 18           1.42          1.64      M HOLLANDE François    349
    ## 19           1.85          2.15      M HOLLANDE François     18
    ## 20           1.47          1.74      M HOLLANDE François   1519
    ## 21           1.18          1.42      M HOLLANDE François     46
    ## 22           2.00          2.56      M HOLLANDE François     68
    ## 23           4.16          4.86      M HOLLANDE François    123
    ## 24           0.00          0.00      M HOLLANDE François     44
    ## 25           5.41          6.45      M HOLLANDE François     12
    ## 26           1.51          1.71      M HOLLANDE François   2959
    ## 27           1.99          2.33      M HOLLANDE François    251
    ## 28           1.03          1.36      M HOLLANDE François   8426
    ## 29           1.97          2.53      M HOLLANDE François     52
    ## 30           1.56          1.80      M HOLLANDE François     24
    ## 31           1.37          1.56      M HOLLANDE François     35
    ## 32           3.13          3.84      M HOLLANDE François    126
    ## 33           1.34          1.64      M HOLLANDE François     96
    ## 34           1.60          1.88      M HOLLANDE François     43
    ## 35           1.16          1.35      M HOLLANDE François    120
    ## 36           1.61          1.83      M HOLLANDE François     44
    ## 37           1.60          1.97      M HOLLANDE François     25
    ## 38           1.31          1.50      M HOLLANDE François     53
    ## 39           1.74          2.03      M HOLLANDE François     38
    ## 40           1.12          1.32      M HOLLANDE François    972
    ## 41           1.65          1.94      M HOLLANDE François    427
    ## 42           2.47          2.90      M HOLLANDE François     48
    ## 43           2.26          3.03      M HOLLANDE François     15
    ## 44           0.98          1.14      M HOLLANDE François     33
    ## 45           1.91          2.37      M HOLLANDE François     63
    ## 46           1.91          2.32      M HOLLANDE François    374
    ## 47           1.67          2.06      M HOLLANDE François    444
    ## 48           2.33          2.94      M HOLLANDE François      9
    ## 49           0.00          0.00      M HOLLANDE François      4
    ## 50           1.85          2.23      M HOLLANDE François    189
    ## 51           2.04          2.29      M HOLLANDE François     52
    ## 52           0.86          1.09      M HOLLANDE François     29
    ## 53           1.41          1.69      M HOLLANDE François     14
    ## 54           2.07          2.38      M HOLLANDE François     55
    ## 55           2.74          3.19      M HOLLANDE François     84
    ## 56           1.44          1.75      M HOLLANDE François    167
    ## 57           1.58          1.88      M HOLLANDE François     82
    ## 58           2.32          2.72      M HOLLANDE François    377
    ## 59           1.52          1.90      M HOLLANDE François   1547
    ## 60           2.70          3.08      M HOLLANDE François     17
    ## 61           2.16          2.65      M HOLLANDE François     23
    ## 62           1.19          1.44      M HOLLANDE François     53
    ## 63           2.04          2.63      M HOLLANDE François     50
    ## 64           4.94          6.06      M HOLLANDE François      2
    ## 65           0.65          0.77      M HOLLANDE François     18
    ## 66           3.65          4.53      M HOLLANDE François     77
    ## 67           2.31          3.03      M HOLLANDE François     48
    ## 68           0.82          0.99      M HOLLANDE François     49
    ## 69           3.66          4.08      M HOLLANDE François     27
    ## 70           1.60          1.90      M HOLLANDE François     44
    ## 71           3.13          3.57      M HOLLANDE François      3
    ## 72           4.35          4.88      M HOLLANDE François      5
    ## 73           0.00          0.00      M HOLLANDE François     21
    ## 74           0.00          0.00      M HOLLANDE François     12
    ## 75           0.00          0.00      M HOLLANDE François     10
    ## 76           2.68          3.45      M HOLLANDE François     19
    ## 77           1.82          2.13      M HOLLANDE François     22
    ## 78           1.50          1.87      M HOLLANDE François    483
    ## 79           1.01          1.36      M HOLLANDE François    572
    ## 80           1.86          2.30      M HOLLANDE François    181
    ## 81           2.16          2.54      M HOLLANDE François     52
    ## 82           1.55          1.88      M HOLLANDE François     39
    ## 83           3.45          3.97      M HOLLANDE François     35
    ## 84           1.08          1.20      M HOLLANDE François    108
    ## 85           1.36          1.60      M HOLLANDE François     41
    ## 86           0.78          0.94      M HOLLANDE François    234
    ## 87           0.56          0.70      M HOLLANDE François     43
    ## 88           1.02          1.24      M HOLLANDE François    893
    ## 89           1.75          1.98      M HOLLANDE François     91
    ## 90           4.44          5.13      M HOLLANDE François     14
    ## 91           0.00          0.00      M HOLLANDE François     18
    ## 92           1.06          1.26      M HOLLANDE François    108
    ## 93           3.46          4.09      M HOLLANDE François     47
    ## 94           2.22          2.40      M HOLLANDE François     37
    ## 95           1.05          1.16      M HOLLANDE François     21
    ## 96           0.82          1.07      M HOLLANDE François     41
    ## 97           2.41          2.90      M HOLLANDE François    128
    ## 98           1.18          1.44      M HOLLANDE François    140
    ## 99           1.45          1.84      M HOLLANDE François    156
    ## 100          1.47          1.71      M HOLLANDE François     69
    ## 101          1.57          1.93      M HOLLANDE François    216
    ## 102          1.45          1.79      M HOLLANDE François     95
    ## 103          2.24          2.63      M HOLLANDE François    203
    ## 104          0.69          0.91      M HOLLANDE François    114
    ## 105          1.31          1.53      M HOLLANDE François     87
    ## 106          1.81          2.27      M HOLLANDE François    286
    ## 107          1.10          1.36      M HOLLANDE François    288
    ## 108          1.97          2.51      M HOLLANDE François    770
    ## 109          2.46          3.13      M HOLLANDE François    400
    ## 110          0.00          0.00      M HOLLANDE François     25
    ## 111          2.11          2.50      M HOLLANDE François     10
    ## 112          0.93          1.05      M HOLLANDE François     13
    ## 113          2.26          2.65      M HOLLANDE François    254
    ## 114          4.09          5.04      M HOLLANDE François     28
    ## 115          0.70          0.94      M HOLLANDE François     15
    ## 116          1.44          1.80      M HOLLANDE François     52
    ## 117          0.69          0.83      M HOLLANDE François   3132
    ##     X..Voix.Ins.9 X..Voix.Exp.9
    ## 1           18.72         21.71
    ## 2           29.76         35.26
    ## 3           14.55         17.20
    ## 4           11.98         14.86
    ## 5           14.84         18.55
    ## 6           24.29         28.81
    ## 7           26.06         30.83
    ## 8           13.79         16.99
    ## 9           22.26         24.74
    ## 10          40.78         47.71
    ## 11          28.86         33.77
    ## 12          10.92         13.03
    ## 13           5.13          5.88
    ## 14          13.33         16.67
    ## 15          23.16         29.30
    ## 16          28.85         40.54
    ## 17          17.76         21.91
    ## 18          27.57         31.81
    ## 19          16.67         19.35
    ## 20          28.96         34.41
    ## 21          18.04         21.80
    ## 22          17.00         21.73
    ## 23          24.36         28.47
    ## 24          28.76         38.94
    ## 25          16.22         19.35
    ## 26          29.11         32.99
    ## 27          27.80         32.47
    ## 28          27.08         35.69
    ## 29          14.61         18.77
    ## 30          18.75         21.62
    ## 31          15.98         18.23
    ## 32          17.17         21.04
    ## 33          18.32         22.43
    ## 34          17.20         20.19
    ## 35          27.91         32.35
    ## 36          17.67         20.18
    ## 37          13.37         16.45
    ## 38          23.14         26.50
    ## 39          22.09         25.68
    ## 40          27.14         32.06
    ## 41          30.54         36.00
    ## 42          29.63         34.78
    ## 43          11.28         15.15
    ## 44          32.35         37.50
    ## 45          24.05         29.86
    ## 46          21.68         26.34
    ## 47          18.14         22.36
    ## 48          20.93         26.47
    ## 49          12.90         14.81
    ## 50          23.25         28.04
    ## 51          17.69         19.85
    ## 52          25.00         31.52
    ## 53           9.86         11.86
    ## 54          18.97         21.83
    ## 55          17.68         20.64
    ## 56          18.51         22.45
    ## 57          21.64         25.71
    ## 58          22.47         26.29
    ## 59          29.41         36.72
    ## 60          22.97         26.15
    ## 61          16.55         20.35
    ## 62          15.77         19.13
    ## 63          25.51         32.89
    ## 64           2.47          3.03
    ## 65          11.61         13.85
    ## 66          18.73         23.26
    ## 67          15.84         20.78
    ## 68          20.08         24.26
    ## 69          16.46         18.37
    ## 70          23.40         27.85
    ## 71           9.38         10.71
    ## 72          10.87         12.20
    ## 73          25.30         35.59
    ## 74          15.38         21.82
    ## 75          24.39         29.41
    ## 76          16.96         21.84
    ## 77          40.00         46.81
    ## 78          18.54         23.11
    ## 79          26.29         35.37
    ## 80          21.00         26.01
    ## 81          22.41         26.40
    ## 82          20.21         24.38
    ## 83          20.11         23.18
    ## 84          29.27         32.53
    ## 85          18.55         21.81
    ## 86          30.47         36.68
    ## 87          24.29         30.28
    ## 88          24.60         30.00
    ## 89          22.69         25.71
    ## 90          31.11         35.90
    ## 91          23.68         32.73
    ## 92          22.98         27.14
    ## 93          14.78         17.47
    ## 94          27.41         29.60
    ## 95          22.11         24.42
    ## 96          16.87         21.93
    ## 97          18.18         21.84
    ## 98          23.65         28.75
    ## 99          28.36         35.86
    ## 100         20.23         23.55
    ## 101         21.24         26.12
    ## 102         22.95         28.27
    ## 103         25.22         29.64
    ## 104         13.12         17.27
    ## 105         16.29         18.95
    ## 106         17.84         22.43
    ## 107         21.10         26.04
    ## 108         18.25         23.26
    ## 109         21.85         27.86
    ## 110         17.61         21.37
    ## 111         10.53         12.50
    ## 112         12.04         13.68
    ## 113         27.31         32.11
    ## 114         16.37         20.14
    ## 115         10.56         14.15
    ## 116         18.71         23.42
    ## 117          8.46         10.17

``` r
df_zones_communes %>% filter(LIBGEO %in% sample_pres)
```

    ##     CODGEO                        LIBGEO DEP REG      EPCI NATURE_EPCI ARR
    ## 1    01122          Cormaranche-en-Bugey  01  84 240100578          CC 011
    ## 2    02097                      Boncourt  02  32 240200576          CC 022
    ## 3    02271                      Dravegny  02  32 200072031          CA 021
    ## 4    02518                     Montlevon  02  32 200072031          CA 021
    ## 5    03139                      Laprugne  03  84 200071363          CA 033
    ## 6    03222                 Saint-Caprais  03  84 240300558          CC 031
    ## 7    03241        Saint-Léopardin-d'Augy  03  84 200071140          CA 032
    ## 8    06075                        Levens  06  93 200030195          ME 062
    ## 9    07314                        Silhac  07  84 200071413          CA 072
    ## 10   09173                       Loubens  09  76 200067791          CA 091
    ## 11   09289              Lorp-Sentaraille  09  76 200067940          CC 093
    ## 12   10121                     Dampierre  10  44 200071777          CC 103
    ## 13   10154           Fontenay-de-Bossery  10  44 200006716          CC 102
    ## 14   11110                     Coustouge  11  76 200035863          CC 113
    ## 15   11261                          Moux  11  76 200035863          CC 113
    ## 16   11282           Peyrefitte-du-Razès  11  76 200043776          CC 112
    ## 17   12036                       Brommat  12  76 200067171          CC 122
    ## 18   12138              Marcillac-Vallon  12  76 241200641          CC 122
    ## 19   16358     Saint-Yrieix-sur-Charente  16  75 200071827          CA 161
    ## 20   17023                         Aujac  17  75 200041689          CC 175
    ## 21   17355               Sainte-Lheurine  17  75 200041523          CC 171
    ## 22   18201                 Saint-Caprais  18  24 241800457          CC 181
    ## 23   21121                Bussy-la-Pesle  21  27 200039055          CC 212
    ## 24   22187                        Plérin  22  53 200069409          CA 224
    ## 25   22256             Quemper-Guézennec  22  53 200067981          CA 222
    ## 26   22278                  Saint-Brieuc  22  53 200069409          CA 224
    ## 27   27002                          Acon  27  28 200066462          CC 273
    ## 28   27081                      Boncourt  27  28 200071454          CA 273
    ## 29   28050                      Boncourt  28  24 200040277          CA 283
    ## 30   28226                    Maillebois  28  24 200040277          CA 283
    ## 31   28275                         Néron  28  24 200069953          CC 283
    ## 32   28407                      Vichères  28  24 200006971          CC 284
    ## 33   29294                     Le Tréhou  29  53 242900801          CC 291
    ## 34   2B238              Poggio-di-Venaco  2B  94 242020071          CC 2B3
    ## 35   30013                    Argilliers  30  76 243000684          CC 302
    ## 36   30022                         Aujac  30  76 200066918          CA 301
    ## 37   30099                          Cros  30  76 200034411          CC 303
    ## 38   30205                 Pougnadoresse  30  76 200034379          CC 302
    ## 39   31182                    Fenouillet  31  76 243100518          ME 313
    ## 40   31239               L'Isle-en-Dodon  31  76 200072643          CC 312
    ## 41   32007                       Ardizas  32  76 200034726          CC 322
    ## 42   32326                     Pouylebon  32  76 243200425          CC 323
    ## 43   32467                 Saint-Caprais  32  76 200042372          CC 321
    ## 44   33250                       Loubens  33  75 200044394          CC 333
    ## 45   38105                       Chirens  38  84 243800984          CA 381
    ## 46   38261                      Morestel  38  84 200068542          CC 382
    ## 47   39136                      Chemenot  39  27 200069615          CC 392
    ## 48   39190                     Dampierre  39  27 243900560          CC 391
    ## 49   39220                     Falletans  39  27 200010650          CA 391
    ## 50   39342                         Monay  39  27 200071595          CC 391
    ## 51   41001                        Ambloy  41  24 200072072          CA 412
    ## 52   41166                         Oisly  41  24 200072064          CC 413
    ## 53   42098                     Fourneaux  42  84 244200630          CC 422
    ## 54   42156                       Neulise  42  84 244200630          CC 422
    ## 55   42306                    Tarentaise  42  84 244200622          CC 423
    ## 56   44048                        Couffé  44  52 244400552          CC 445
    ## 57   44210                       Trignac  44  52 244400644          CA 443
    ## 58   46250                 Saint-Caprais  46  76 200035327          CC 463
    ## 59   47011                         Anthé  47  75 200068930          CC 473
    ## 60   47309                    Tombeboeuf  47  75 244701405          CC 473
    ## 61   50192                     Fourneaux  50  28 200066389          CA 504
    ## 62   51621             Vienne-le-Château  51  44 200042703          CC 515
    ## 63   52163                     Dampierre  52  44 200072999          CC 522
    ## 64   53138                    Longuefuye  53  52 245300447          CC 531
    ## 65   54082                      Boncourt  54  44 200070845          CC 541
    ## 66   54172         Doncourt-lès-Longuyon  54  44 200043693          CC 541
    ## 67   55044                       Belrain  55  44 200066140          CC 552
    ## 68   55323         Martincourt-sur-Meuse  55  44 200066132          CC 553
    ## 69   55544                      Velosnes  55  44 245501259          CC 553
    ## 70   58014                     Arzembouy  58  27 200068088          CC 584
    ## 71   58043                Bussy-la-Pesle  58  27 200067692          CC 582
    ## 72   58162                    Menestreau  58  27 200067916          CC 584
    ## 73   58284                         Talon  58  27 200067692          CC 582
    ## 74   59037            Avesnes-les-Aubert  59  32 200030633          CC 592
    ## 75   59361                      Lourches  59  32 200042190          CA 596
    ## 76   60387       Marseille-en-Beauvaisis  60  32 246000848          CC 601
    ## 77   61451 Saint-Quentin-les-Chardonnets  61  28 200071520          CC 612
    ## 78   62084                         Barly  62  32 200069482          CC 621
    ## 79   62475                       Ivergny  62  32 200069482          CC 621
    ## 80   62676                       Quernes  62  32 200072460          CA 622
    ## 81   62797                     Siracourt  62  32 200069672          CC 621
    ## 82   63048                  Bourg-Lastic  63  84 200071215          CC 634
    ## 83   63129                          Cros  63  84 200069169          CC 633
    ## 84   64132                       Bizanos  64  75 200067254          CA 643
    ## 85   64183                  Caubios-Loos  64  75 200067239          CC 643
    ## 86   65217                      Hautaget  65  76 200070829          CC 652
    ## 87   66077                    Fenouillet  66  76 246600423          CC 663
    ## 88   66165                         Rodès  66  76 246600415          CC 663
    ## 89   69138                    Montromant  69  84 200066587          CC 692
    ## 90   70262                  Genevreuille  70  27 247000664          CC 701
    ## 91   70479            Sauvigney-lès-Gray  70  27 200036549          CC 702
    ## 92   70572              Vitrey-sur-Mance  70  27 200036150          CC 702
    ## 93   71404                  Saint-Désert  71  27 247100589          CA 712
    ## 94   73117                     Fourneaux  73  84 200070340          CC 733
    ## 95   74290                    Vallorcine  74  84 200023372          CC 742
    ## 96   76298                    Ganzeville  76  28 200069821          CA 762
    ## 97   76514        Quévreville-la-Poterie  76  28 200023414          ME 763
    ## 98   77180              Férolles-Attilly  77  11 200023125          CC 775
    ## 99   77369                        Poincy  77  11 200072130          CA 771
    ## 100  78238               Flins-sur-Seine  78  11 200059889          CU 781
    ## 101  78299                   Hardricourt  78  11 200059889          CU 781
    ## 102  78380                         Maule  78  11 200034130          CC 783
    ## 103  79171              Mauzé-Thouarsais  79  75 247900798          CC 791
    ## 104  80055                         Barly  80  32 200070951          CC 802
    ## 105  80667                     Remaugies  80  32 200070977          CC 803
    ## 106  81229                    Roquevidal  81  76 200034023          CC 812
    ## 107  86235      Saint-Maurice-la-Clouère  86  75 200070035          CC 862
    ## 108  88318                      Moyemont  88  44 200005957          CC 881
    ## 109  88363                       Punerot  88  44 200068559          CC 882
    ## 110  89479                   Vincelottes  89  27 200067114          CA 891
    ## 111  92051             Neuilly-sur-Seine  92  11 200054781          ME 922
    ##       CV ZE2010 UU2010 TUU2014 TDUU2014 AU2010 TAU2014 CATAEU2010 BV2012
    ## 1   0110   8203  01000       0        5    540       1        222  01185
    ## 2   0206   2204  02000       0        4    000       0        400  51454
    ## 3   0205   2201  02000       0        3    029       8        112  51250
    ## 4   0204   2201  02000       0        4    997       0        120  02168
    ## 5   0308   8303  03000       0        4    000       0        400  03165
    ## 6   0307   8301  03000       0        2    000       0        400  03084
    ## 7   0302   8302  03000       0        4    000       0        400  03036
    ## 8   0624   9307  06108       1       14    007       9        112  06088
    ## 9   0717   8208  07000       0        4    000       0        400  07338
    ## 10  0913   7301  09000       0        4    997       0        120  09225
    ## 11  0911   7302  09203       2       22    267       2        211  09261
    ## 12  1002   2102  10000       0        4    000       0        400  10006
    ## 13  1007   2102  10000       0        2    998       0        300  10268
    ## 14  1107   9103  11000       0        3    000       0        400  11203
    ## 15  1118   9101  11000       0        5    998       0        300  11203
    ## 16  1114   9102  11000       0        1    000       0        400  09194
    ## 17  1201   7304  12000       0        5    000       0        400  12164
    ## 18  1221   7304  12000       0        6    104       6        112  12138
    ## 19  1614   5403  16601       6       61    049       7        111  16015
    ## 20  1702   5405  17000       0        4    000       0        400  17224
    ## 21  1707   5405  17000       0        5    000       0        400  17197
    ## 22  1817   2401  18000       0        5    057       7        112  18207
    ## 23  2123   2603  21000       0        2    025       8        112  21231
    ## 24  2218   5305  22501       5       52    051       7        111  22278
    ## 25  2201   5302  22000       0        6    998       0        300  22162
    ## 26  2299   5305  22501       5       52    051       7        111  22278
    ## 27  2722   2302  27000       0        4    997       0        120  28348
    ## 28  2711   2302  27000       0        3    077       7        112  27448
    ## 29  2801   2406  00361       3       32    001      10        112  27230
    ## 30  2814   2406  28000       0        5    997       0        120  27679
    ## 31  2810   2406  28000       0        5    001      10        112  28227
    ## 32  2813     54  28000       0        4    263       2        212  28280
    ## 33  2921   5306  29000       0        5    998       0        300  29103
    ## 34  2B11   9406  2B000       0        4    393       1        222  2B096
    ## 35  3016   9106  30000       0        4    998       0        300  30212
    ## 36  3008   9104  30000       0        3    000       0        400  30037
    ## 37  3015   9110  30000       0        4    000       0        400  30263
    ## 38  3020   9105  30000       0        4    997       0        120  30334
    ## 39  3105     61  31701       7       73    004       9        111  31555
    ## 40  3106     61  31000       0        6    000       0        400  31239
    ## 41  3211     61  32000       0        4    004       9        112  31098
    ## 42  3216   7307  32000       0        3    000       0        400  32256
    ## 43  3205   7307  32000       0        3    997       0        120  32147
    ## 44  3327   7204  33000       0        4    998       0        300  33352
    ## 45  3808   8210  38701       7       73    010       9        111  38185
    ## 46  3817   8209  38129       3       32    002       9        112  38261
    ## 47  3903   4306  39000       0        1    997       0        120  39434
    ## 48  3910   4301  39103       1       11    041       8        112  25527
    ## 49  3902   4305  39000       0        4    124       6        112  39198
    ## 50  3903   4306  39000       0        3    997       0        120  39434
    ## 51  4106   2415  41000       0        3    997       0        120  41149
    ## 52  4107   2413  41000       0        4    998       0        300  41059
    ## 53  4204   8212  42000       0        5    998       0        300  69006
    ## 54  4204   8212  42000       0        6    998       0        300  42011
    ## 55  4208     60  42000       0        4    017       9        112  42218
    ## 56  4401   5203  44000       0        7    008       9        112  44003
    ## 57  4427   5204  44601       6       62    046       8        111  44184
    ## 58  4615   7308  46000       0        2    000       0        400  46225
    ## 59  4709   7211  47000       0        4    997       0        120  47323
    ## 60  4712   7210  47000       0        4    998       0        300  47168
    ## 61  5009   2510  50000       0        3    998       0        300  50601
    ## 62  5101   2103  51000       0        5    000       0        400  51507
    ## 63  5211   2106  52000       0        4    998       0        300  52332
    ## 64  5301   5209  53000       0        4    227       4        212  53062
    ## 65  5404   4107  54000       0        3    998       0        300  54273
    ## 66  5411   4101  54000       0        4    115       6        112  54322
    ## 67  5508   4105  55000       0        1    182       4        112  55029
    ## 68  5514   4106  55000       0        2    998       0        300  55502
    ## 69  5511   4101  55000       0        3    115       6        112  54322
    ## 70  5801     52  58000       0        2    000       0        400  58218
    ## 71  5804   2605  58000       0        2    000       0        400  58079
    ## 72  5815     52  58000       0        3    000       0        400  58086
    ## 73  5803     52  58000       0        1    000       0        400  58083
    ## 74  5911   3116  59124       1       13    997       0        120  59139
    ## 75  5914   3115  59701       7       72    026       8        111  59606
    ## 76  6011   2207  60000       0        6    067       7        112  60286
    ## 77  6111   2512  61000       0        4    997       0        120  61486
    ## 78  6207   3121  62000       0        4    062       7        112  62063
    ## 79  6207   3121  62000       0        4    997       0        120  62361
    ## 80  6201   3123  62000       0        4    027       8        112  62119
    ## 81  6238   3121  62000       0        4    364       1        212  62767
    ## 82  6328   8310  63000       0        5    000       0        400  63047
    ## 83  6329   8310  63000       0        3    000       0        400  19028
    ## 84  6420   7214  64601       6       62    042       8        111  64445
    ## 85  6425   7214  64000       0        5    042       8        112  64445
    ## 86  6515   7310  65000       0        2    998       0        300  31390
    ## 87  6615   9115  66000       0        2    000       0        400  11304
    ## 88  6602   9116  66000       0        5    997       0        120  66088
    ## 89  6902   8214  69000       0        4    002       9        112  69220
    ## 90  7007   4309  70000       0        3    997       0        120  70310
    ## 91  7002   4308  70000       0        3    264       2        212  70279
    ## 92  7005   4309  70000       0        4    000       0        400  70292
    ## 93  7117   2608  71000       0        5    060       7        112  71221
    ## 94  7310   8217  73109       1       13    567       1        221  73157
    ## 95  7410   8221  74000       0        4    998       0        300  74056
    ## 96  7611   2306  76000       0        5    207       4        212  76259
    ## 97  7606   2307  76101       1       11    012       9        112  76540
    ## 98  7716   1117  77000       0        6    001      10        112  75056
    ## 99  7706   1104  77501       5       52    001      10        112  77284
    ## 100 7801   1111  00851       8       80    001      10        111  75056
    ## 101 7811   1111  00851       8       80    001      10        111  75056
    ## 102 7801   1111  78301       3       31    001      10        112  78380
    ## 103 7916   5401  79000       0        7    211       4        212  79329
    ## 104 8014   2210  80000       0        3    998       0        300  80253
    ## 105 8022   2210  80000       0        3    998       0        300  80561
    ## 106 8115     61  81000       0        3    997       0        120  81140
    ## 107 8610   5413  86105       1       13    039       8        112  86103
    ## 108 8803   4112  88000       0        4    998       0        300  88367
    ## 109 8810   4103  88000       0        3    997       0        120  88321
    ## 110 8921   2612  89000       0        4    096       6        112  89024
    ## 111 9221   1101  00851       8       80    001      10        111  75056

``` r
df_zones_communes$dept<-str_sub(df_zones_communes$CODGEO,1,end=2) %>% as.numeric()
```

    ## Warning in function_list[[k]](value): NAs introduced by coercion

``` r
df_zones_communes$communes<-str_sub(df_zones_communes$CODGEO,3)%>% as.numeric()
str(df_zones_communes)
```

    ## 'data.frame':    35416 obs. of  18 variables:
    ##  $ CODGEO     : Factor w/ 35416 levels "01001","01002",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ LIBGEO     : Factor w/ 33057 levels "Aast","Abainville",..: 13379 13381 433 435 450 468 472 565 618 775 ...
    ##  $ DEP        : Factor w/ 101 levels "01","02","03",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ REG        : int  84 84 84 84 84 84 84 84 84 84 ...
    ##  $ EPCI       : Factor w/ 1268 levels "200000172","200000438",..: 502 726 726 230 164 726 726 164 595 238 ...
    ##  $ NATURE_EPCI: Factor w/ 5 levels "CA","CC","CU",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ ARR        : Factor w/ 334 levels "011","012","013",..: 2 1 1 2 1 1 1 1 1 4 ...
    ##  $ CV         : Factor w/ 2068 levels "0101","0102",..: 8 1 1 22 4 1 1 4 10 14 ...
    ##  $ ZE2010     : int  8213 8201 8201 8213 8216 8201 8201 8216 8219 8203 ...
    ##  $ UU2010     : Factor w/ 2389 levels "00151","00152",..: 91 91 123 91 91 91 123 91 91 91 ...
    ##  $ TUU2014    : int  0 0 3 0 0 0 3 0 0 0 ...
    ##  $ TDUU2014   : int  5 4 32 6 3 7 32 4 6 4 ...
    ##  $ AU2010     : Factor w/ 794 levels "000","001","002",..: 771 3 3 3 772 3 3 295 772 166 ...
    ##  $ TAU2014    : int  0 9 9 9 0 9 9 2 0 5 ...
    ##  $ CATAEU2010 : int  120 112 112 112 300 112 112 212 300 112 ...
    ##  $ BV2012     : Factor w/ 1664 levels "01004","01033",..: 5 1 1 1259 3 1 1 3 1357 14 ...
    ##  $ dept       : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ communes   : num  1 2 4 5 6 7 8 9 10 11 ...

``` r
df_pres_merged<-merge(df_pres,df_zones_communes, by.y = c('dept','communes'),by.x = c('Code.du.département','Code.de.la.commune'),all.x = TRUE)
#str(df_pres_merged)
#df_pres_merged[is.na(df_pres_merged$ZE2010),]$Libellé.de.la.commune
```

``` r
nrow(df_pres_merged[is.na(df_pres_merged$ZE2010),])/nrow(df_pres)*100
```

    ## [1] 5.090919

``` r
df_zones_communes %>% filter(grepl("Song",LIBGEO))
```

    ##   CODGEO   LIBGEO DEP REG      EPCI NATURE_EPCI ARR   CV ZE2010 UU2010
    ## 1  39518 Songeson  39  27 243900719          CC 392 3915   4306  39000
    ## 2  51552    Songy  51  44 200034718          CC 514 5123   2107  51000
    ## 3  60623 Songeons  60  32 246000848          CC 601 6011   2207  60000
    ##   TUU2014 TDUU2014 AU2010 TAU2014 CATAEU2010 BV2012 dept communes
    ## 1       0        2    000       0        400  39154   39      518
    ## 2       0        4    997       0        120  51649   51      552
    ## 3       0        6    067       7        112  76312   60      623

``` r
df_pres %>% filter(grepl("Natt",Libellé.de.la.commune))
```

    ##   Code.du.département Libellé.du.département Code.de.la.commune
    ## 1                   1                    AIN                271
    ##   Libellé.de.la.commune Inscrits Abstentions X..Abs.Ins Votants X..Vot.Ins
    ## 1              Nattages      452          50      11.06     402      88.94
    ##   Blancs.et.nuls X..BlNuls.Ins X..BlNuls.Vot Exprimés X..Exp.Ins
    ## 1              8          1.77          1.99      394      87.17
    ##   X..Exp.Vot  Sexe  Nom Prénom Voix X..Voix.Ins X..Voix.Exp Sexe.1  Nom.1
    ## 1      98.01 FALSE JOLY    Eva   19         4.2        4.82  FALSE LE PEN
    ##   Prénom.1 Voix.1 X..Voix.Ins.1 X..Voix.Exp.1 Sexe.2   Nom.2 Prénom.2
    ## 1   Marine     75         16.59         19.04      M SARKOZY  Nicolas
    ##   Voix.2 X..Voix.Ins.2 X..Voix.Exp.2 Sexe.3     Nom.3 Prénom.3 Voix.3
    ## 1    108         23.89         27.41      M MÉLENCHON Jean-Luc     53
    ##   X..Voix.Ins.3 X..Voix.Exp.3 Sexe.4  Nom.4 Prénom.4 Voix.4 X..Voix.Ins.4
    ## 1         11.73         13.45      M POUTOU Philippe      3          0.66
    ##   X..Voix.Exp.4 Sexe.5   Nom.5 Prénom.5 Voix.5 X..Voix.Ins.5 X..Voix.Exp.5
    ## 1          0.76  FALSE ARTHAUD Nathalie      2          0.44          0.51
    ##   Sexe.6     Nom.6 Prénom.6 Voix.6 X..Voix.Ins.6 X..Voix.Exp.6 Sexe.7
    ## 1      M CHEMINADE  Jacques      0             0             0      M
    ##    Nom.7 Prénom.7 Voix.7 X..Voix.Ins.7 X..Voix.Exp.7 Sexe.8         Nom.8
    ## 1 BAYROU François     28          6.19          7.11      M DUPONT-AIGNAN
    ##   Prénom.8 Voix.8 X..Voix.Ins.8 X..Voix.Exp.8 Sexe.9    Nom.9 Prénom.9
    ## 1  Nicolas      7          1.55          1.78      M HOLLANDE François
    ##   Voix.9 X..Voix.Ins.9 X..Voix.Exp.9
    ## 1     99          21.9         25.13

``` r
df_pres_merged_chomage<-merge(df_pres_merged,df_chomage,by = 'ZE2010',all.x=T)
dim(df_pres_merged_chomage)
```

    ## [1] 38664   150

``` r
sum(is.na(df_pres_merged_chomage$X2016.T4))
```

    ## [1] 969

``` r
df_pres_merged_chomage <- df_pres_merged_chomage %>% filter(!is.na(X2016.T4))
dim(df_pres_merged_chomage)
```

    ## [1] 37695   150

``` r
head(df_pres_merged_chomage)
```

    ##   ZE2010 Code.du.département Code.de.la.commune Libellé.du.département
    ## 1     50                  32                 36                   GERS
    ## 2     50                  40                 57                 LANDES
    ## 3     50                  40                215                 LANDES
    ## 4     50                  40                177                 LANDES
    ## 5     50                  40                 55                 LANDES
    ## 6     50                  40                178                 LANDES
    ##   Libellé.de.la.commune Inscrits Abstentions X..Abs.Ins Votants X..Vot.Ins
    ## 1           Beaumarchés      503          78      15.51     425      84.49
    ## 2                Buanes      217          36      16.59     181      83.41
    ## 3           Ousse-Suzan      210          18       8.57     192      91.43
    ## 4                Maylis      278          35      12.59     243      87.41
    ## 5    Bretagne-de-Marsan      935         115      12.30     820      87.70
    ## 6            Mazerolles      586          90      15.36     496      84.64
    ##   Blancs.et.nuls X..BlNuls.Ins X..BlNuls.Vot Exprimés X..Exp.Ins
    ## 1              8          1.59          1.88      417      82.90
    ## 2              4          1.84          2.21      177      81.57
    ## 3              5          2.38          2.60      187      89.05
    ## 4              8          2.88          3.29      235      84.53
    ## 5             11          1.18          1.34      809      86.52
    ## 6              5          0.85          1.01      491      83.79
    ##   X..Exp.Vot  Sexe  Nom Prénom Voix X..Voix.Ins X..Voix.Exp Sexe.1  Nom.1
    ## 1      98.12 FALSE JOLY    Eva   15        2.98        3.60  FALSE LE PEN
    ## 2      97.79 FALSE JOLY    Eva    1        0.46        0.56  FALSE LE PEN
    ## 3      97.40 FALSE JOLY    Eva    1        0.48        0.53  FALSE LE PEN
    ## 4      96.71 FALSE JOLY    Eva    4        1.44        1.70  FALSE LE PEN
    ## 5      98.66 FALSE JOLY    Eva   14        1.50        1.73  FALSE LE PEN
    ## 6      98.99 FALSE JOLY    Eva   12        2.05        2.44  FALSE LE PEN
    ##   Prénom.1 Voix.1 X..Voix.Ins.1 X..Voix.Exp.1 Sexe.2   Nom.2 Prénom.2
    ## 1   Marine     56         11.13         13.43      M SARKOZY  Nicolas
    ## 2   Marine     38         17.51         21.47      M SARKOZY  Nicolas
    ## 3   Marine     18          8.57          9.63      M SARKOZY  Nicolas
    ## 4   Marine     14          5.04          5.96      M SARKOZY  Nicolas
    ## 5   Marine    128         13.69         15.82      M SARKOZY  Nicolas
    ## 6   Marine     64         10.92         13.03      M SARKOZY  Nicolas
    ##   Voix.2 X..Voix.Ins.2 X..Voix.Exp.2 Sexe.3     Nom.3 Prénom.3 Voix.3
    ## 1     86         17.10         20.62      M MÉLENCHON Jean-Luc     61
    ## 2     39         17.97         22.03      M MÉLENCHON Jean-Luc     21
    ## 3     30         14.29         16.04      M MÉLENCHON Jean-Luc     20
    ## 4     64         23.02         27.23      M MÉLENCHON Jean-Luc     16
    ## 5    193         20.64         23.86      M MÉLENCHON Jean-Luc     87
    ## 6    116         19.80         23.63      M MÉLENCHON Jean-Luc     58
    ##   X..Voix.Ins.3 X..Voix.Exp.3 Sexe.4  Nom.4 Prénom.4 Voix.4 X..Voix.Ins.4
    ## 1         12.13         14.63      M POUTOU Philippe     10          1.99
    ## 2          9.68         11.86      M POUTOU Philippe      3          1.38
    ## 3          9.52         10.70      M POUTOU Philippe      2          0.95
    ## 4          5.76          6.81      M POUTOU Philippe      3          1.08
    ## 5          9.30         10.75      M POUTOU Philippe     14          1.50
    ## 6          9.90         11.81      M POUTOU Philippe      9          1.54
    ##   X..Voix.Exp.4 Sexe.5   Nom.5 Prénom.5 Voix.5 X..Voix.Ins.5 X..Voix.Exp.5
    ## 1          2.40  FALSE ARTHAUD Nathalie      4          0.80          0.96
    ## 2          1.69  FALSE ARTHAUD Nathalie      3          1.38          1.69
    ## 3          1.07  FALSE ARTHAUD Nathalie      0          0.00          0.00
    ## 4          1.28  FALSE ARTHAUD Nathalie      1          0.36          0.43
    ## 5          1.73  FALSE ARTHAUD Nathalie      0          0.00          0.00
    ## 6          1.83  FALSE ARTHAUD Nathalie      0          0.00          0.00
    ##   Sexe.6     Nom.6 Prénom.6 Voix.6 X..Voix.Ins.6 X..Voix.Exp.6 Sexe.7
    ## 1      M CHEMINADE  Jacques      2          0.40          0.48      M
    ## 2      M CHEMINADE  Jacques      1          0.46          0.56      M
    ## 3      M CHEMINADE  Jacques      1          0.48          0.53      M
    ## 4      M CHEMINADE  Jacques      0          0.00          0.00      M
    ## 5      M CHEMINADE  Jacques      3          0.32          0.37      M
    ## 6      M CHEMINADE  Jacques      1          0.17          0.20      M
    ##    Nom.7 Prénom.7 Voix.7 X..Voix.Ins.7 X..Voix.Exp.7 Sexe.8         Nom.8
    ## 1 BAYROU François     52         10.34         12.47      M DUPONT-AIGNAN
    ## 2 BAYROU François      8          3.69          4.52      M DUPONT-AIGNAN
    ## 3 BAYROU François     21         10.00         11.23      M DUPONT-AIGNAN
    ## 4 BAYROU François     38         13.67         16.17      M DUPONT-AIGNAN
    ## 5 BAYROU François     97         10.37         11.99      M DUPONT-AIGNAN
    ## 6 BAYROU François     82         13.99         16.70      M DUPONT-AIGNAN
    ##   Prénom.8 Voix.8 X..Voix.Ins.8 X..Voix.Exp.8 Sexe.9    Nom.9 Prénom.9
    ## 1  Nicolas      3          0.60          0.72      M HOLLANDE François
    ## 2  Nicolas      3          1.38          1.69      M HOLLANDE François
    ## 3  Nicolas      2          0.95          1.07      M HOLLANDE François
    ## 4  Nicolas      6          2.16          2.55      M HOLLANDE François
    ## 5  Nicolas     13          1.39          1.61      M HOLLANDE François
    ## 6  Nicolas     12          2.05          2.44      M HOLLANDE François
    ##   Voix.9 X..Voix.Ins.9 X..Voix.Exp.9 CODGEO             LIBGEO DEP REG.x
    ## 1    128         25.45         30.70  32036        Beaumarchés  32    76
    ## 2     60         27.65         33.90  40057             Buanes  40    75
    ## 3     92         43.81         49.20  40215        Ousse-Suzan  40    75
    ## 4     89         32.01         37.87  40177             Maylis  40    75
    ## 5    260         27.81         32.14  40055 Bretagne-de-Marsan  40    75
    ## 6    137         23.38         27.90  40178         Mazerolles  40    75
    ##        EPCI NATURE_EPCI ARR   CV UU2010 TUU2014 TDUU2014 AU2010 TAU2014
    ## 1 243200508          CC 323 3216  32000       0        5    000       0
    ## 2 200030435          CC 402 4001  40000       0        4    997       0
    ## 3 244000691          CC 402 4013  40000       0        4    998       0
    ## 4 200069631          CC 401 4004  40000       0        4    000       0
    ## 5 244000808          CA 402 4011  40000       0        6    122       6
    ## 6 244000808          CA 402 4011  40000       0        5    122       6
    ##   CATAEU2010 BV2012      LIBZE2010 REG.y        LIBREG X2003.T1 X2003.T2
    ## 1        400  32319 Mont-de-Marsan     0 interrégional      6.1      6.1
    ## 2        120  40001 Mont-de-Marsan     0 interrégional      6.1      6.1
    ## 3        300  40313 Mont-de-Marsan     0 interrégional      6.1      6.1
    ## 4        400  40201 Mont-de-Marsan     0 interrégional      6.1      6.1
    ## 5        112  40192 Mont-de-Marsan     0 interrégional      6.1      6.1
    ## 6        112  40192 Mont-de-Marsan     0 interrégional      6.1      6.1
    ##   X2003.T3 X2003.T4 X2004.T1 X2004.T2 X2004.T3 X2004.T4 X2005.T1 X2005.T2
    ## 1      5.9      6.4      6.5      6.2      6.2      6.5      6.2      6.3
    ## 2      5.9      6.4      6.5      6.2      6.2      6.5      6.2      6.3
    ## 3      5.9      6.4      6.5      6.2      6.2      6.5      6.2      6.3
    ## 4      5.9      6.4      6.5      6.2      6.2      6.5      6.2      6.3
    ## 5      5.9      6.4      6.5      6.2      6.2      6.5      6.2      6.3
    ## 6      5.9      6.4      6.5      6.2      6.2      6.5      6.2      6.3
    ##   X2005.T3 X2005.T4 X2006.T1 X2006.T2 X2006.T3 X2006.T4 X2007.T1 X2007.T2
    ## 1      6.6      6.5      6.6      6.4      6.2      5.9        6      5.8
    ## 2      6.6      6.5      6.6      6.4      6.2      5.9        6      5.8
    ## 3      6.6      6.5      6.6      6.4      6.2      5.9        6      5.8
    ## 4      6.6      6.5      6.6      6.4      6.2      5.9        6      5.8
    ## 5      6.6      6.5      6.6      6.4      6.2      5.9        6      5.8
    ## 6      6.6      6.5      6.6      6.4      6.2      5.9        6      5.8
    ##   X2007.T3 X2007.T4 X2008.T1 X2008.T2 X2008.T3 X2008.T4 X2009.T1 X2009.T2
    ## 1      5.6      5.1      4.9      5.1      5.3      5.5        6      6.4
    ## 2      5.6      5.1      4.9      5.1      5.3      5.5        6      6.4
    ## 3      5.6      5.1      4.9      5.1      5.3      5.5        6      6.4
    ## 4      5.6      5.1      4.9      5.1      5.3      5.5        6      6.4
    ## 5      5.6      5.1      4.9      5.1      5.3      5.5        6      6.4
    ## 6      5.6      5.1      4.9      5.1      5.3      5.5        6      6.4
    ##   X2009.T3 X2009.T4 X2010.T1 X2010.T2 X2010.T3 X2010.T4 X2011.T1 X2011.T2
    ## 1      6.4      6.6      6.6      6.6      6.8      6.8      6.9      6.9
    ## 2      6.4      6.6      6.6      6.6      6.8      6.8      6.9      6.9
    ## 3      6.4      6.6      6.6      6.6      6.8      6.8      6.9      6.9
    ## 4      6.4      6.6      6.6      6.6      6.8      6.8      6.9      6.9
    ## 5      6.4      6.6      6.6      6.6      6.8      6.8      6.9      6.9
    ## 6      6.4      6.6      6.6      6.6      6.8      6.8      6.9      6.9
    ##   X2011.T3 X2011.T4 X2012.T1 X2012.T2 X2012.T3 X2012.T4 X2013.T1 X2013.T2
    ## 1        7      7.2      7.4      7.5      7.5      7.8      7.9      7.8
    ## 2        7      7.2      7.4      7.5      7.5      7.8      7.9      7.8
    ## 3        7      7.2      7.4      7.5      7.5      7.8      7.9      7.8
    ## 4        7      7.2      7.4      7.5      7.5      7.8      7.9      7.8
    ## 5        7      7.2      7.4      7.5      7.5      7.8      7.9      7.8
    ## 6        7      7.2      7.4      7.5      7.5      7.8      7.9      7.8
    ##   X2013.T3 X2013.T4 X2014.T1 X2014.T2 X2014.T3 X2014.T4 X2015.T1 X2015.T2
    ## 1      7.9      7.8      7.9      7.8        8      8.1        8      8.2
    ## 2      7.9      7.8      7.9      7.8        8      8.1        8      8.2
    ## 3      7.9      7.8      7.9      7.8        8      8.1        8      8.2
    ## 4      7.9      7.8      7.9      7.8        8      8.1        8      8.2
    ## 5      7.9      7.8      7.9      7.8        8      8.1        8      8.2
    ## 6      7.9      7.8      7.9      7.8        8      8.1        8      8.2
    ##   X2015.T3 X2015.T4 X2016.T1 X2016.T2 X2016.T3 X2016.T4
    ## 1      8.1      7.9      7.9      7.9      7.9      7.7
    ## 2      8.1      7.9      7.9      7.9      7.9      7.7
    ## 3      8.1      7.9      7.9      7.9      7.9      7.7
    ## 4      8.1      7.9      7.9      7.9      7.9      7.7
    ## 5      8.1      7.9      7.9      7.9      7.9      7.7
    ## 6      8.1      7.9      7.9      7.9      7.9      7.7

``` r
ggpairs(df_pres_merged_chomage,columns = c("X2016.T4","X..Voix.Exp.1","X..Voix.Exp.2","X..Voix.Exp.3"))
```

![](elections_france_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
# Emplacement de l'archive décompressée, à remplacer par le votre
pathToShp <- "data/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/"
# Description des données via orgInfo
# Attention à ne pas mettre l'extension à la fin du nom
ogrInfo(dsn = pathToShp,layer="COMMUNE")
```

    ## Source: "data/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/", layer: "COMMUNE"
    ## Driver: ESRI Shapefile; number of rows: 35798 
    ## Feature type: wkbPolygon with 2 dimensions
    ## Extent: (99217.1 6049646) - (1242417 7110480)
    ## CRS: +proj=lcc +lat_1=44 +lat_2=49 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs  
    ## LDID: 87 
    ## Number of fields: 17 
    ##          name type length typeName
    ## 1   ID_GEOFLA    4     24   String
    ## 2    CODE_COM    4      3   String
    ## 3   INSEE_COM    4      5   String
    ## 4     NOM_COM    4     50   String
    ## 5      STATUT    4     25   String
    ## 6  X_CHF_LIEU    0      7  Integer
    ## 7  Y_CHF_LIEU    0      7  Integer
    ## 8  X_CENTROID    0      7  Integer
    ## 9  Y_CENTROID    0      7  Integer
    ## 10    Z_MOYEN    0      4  Integer
    ## 11 SUPERFICIE    2     12     Real
    ## 12 POPULATION    0     10  Integer
    ## 13   CODE_ARR    4      1   String
    ## 14  CODE_DEPT    4      2   String
    ## 15   NOM_DEPT    4     30   String
    ## 16   CODE_REG    4      2   String
    ## 17    NOM_REG    4     35   String

``` r
# Import via la fonction readOGR de rgdal
# Pour info, d'autres outils existent (ex : fonction readShapeSpatial() du package maptools)
comm <- readOGR(dsn = pathToShp, layer="COMMUNE", stringsAsFactors=FALSE)
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "data/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/", layer: "COMMUNE"
    ## with 35798 features
    ## It has 17 fields

``` r
# Description de la structure globale
str(comm, 2)
```

    ## Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
    ##   ..@ data       :'data.frame':  35798 obs. of  17 variables:
    ##   ..@ polygons   :List of 35798
    ##   .. .. [list output truncated]
    ##   ..@ plotOrder  : int [1:35798] 27247 2813 25023 1026 7305 33712 16293 10279 14244 34925 ...
    ##   ..@ bbox       : num [1:2, 1:2] 99217 6049646 1242417 7110480
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot

``` r
# Description de la structure des données associées 
str(comm@data, 2)
```

    ## 'data.frame':    35798 obs. of  17 variables:
    ##  $ ID_GEOFLA : chr  "COMMUNE00000000000000001" "COMMUNE00000000000000002" "COMMUNE00000000000000003" "COMMUNE00000000000000004" ...
    ##  $ CODE_COM  : chr  "216" "033" "009" "225" ...
    ##  $ INSEE_COM : chr  "32216" "47033" "32009" "38225" ...
    ##  $ NOM_COM   : chr  "LOURTIES-MONBRUN" "BOUDY-DE-BEAUREGARD" "ARMOUS-ET-CAU" "AUTRANS-MEAUDRE EN VERCORS" ...
    ##  $ STATUT    : chr  "Commune simple" "Commune simple" "Commune simple" "Commune simple" ...
    ##  $ X_CHF_LIEU: int  500820 516424 472979 898640 640049 824246 461332 746925 1028827 782215 ...
    ##  $ Y_CHF_LIEU: int  6264958 6384852 6278963 6450689 7028672 6908952 6300782 6790005 6315717 6538794 ...
    ##  $ X_CENTROID: int  500515 515575 473004 898625 640115 824391 460721 747181 1027327 782159 ...
    ##  $ Y_CENTROID: int  6265413 6385938 6278937 6451597 7029900 6908954 6302268 6789569 6316879 6538837 ...
    ##  $ Z_MOYEN   : int  252 112 221 1234 79 125 134 167 752 438 ...
    ##  $ SUPERFICIE: num  966 1019 932 3371 1023 ...
    ##  $ POPULATION: int  139 414 95 2973 178 80 97 362 296 901 ...
    ##  $ CODE_ARR  : chr  "3" "3" "3" "1" ...
    ##  $ CODE_DEPT : chr  "32" "47" "32" "38" ...
    ##  $ NOM_DEPT  : chr  "GERS" "LOT-ET-GARONNE" "GERS" "ISERE" ...
    ##  $ CODE_REG  : chr  "76" "75" "76" "84" ...
    ##  $ NOM_REG   : chr  "LANGUEDOC-ROUSSILLON-MIDI-PYRENEES" "AQUITAINE-LIMOUSIN-POITOU-CHARENTES" "LANGUEDOC-ROUSSILLON-MIDI-PYRENEES" "AUVERGNE-RHONE-ALPES" ...

``` r
# Représentation de toutes les communes d'IDF
plot(comm[comm$CODE_REG=="11",])
# Ajout sur le graphique précédent (via add = TRUE) des communes d'intérêt, remplies en rouge (via col="red") 
plot(comm[comm$CODE_REG=="11" & comm$POPULATION>5000,], col="red",add=T)
```

![](elections_france_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
comm@data <- left_join(comm@data, df_pres_merged_chomage, by = c("INSEE_COM" = "CODGEO"))
```

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## factor and character vector, coercing into character vector

``` r
# Filtrage des données pour ne garder que la région Ile-De-France
comm <- comm[comm@data$CODE_REG=="11",]
```

``` r
# Découpage du temps de trajet en 5 classes via la méthodes des quantiles : idenfication des bornes (breaks, ou brks)
classSuffrages <- classIntervals(comm@data$X..Voix.Exp.1, 5, style = "quantile")
```

    ## Warning in classIntervals(comm@data$X..Voix.Exp.1, 5, style = "quantile"):
    ## var has missing values, omitted in finding classes

``` r
# Choix d'une palette de couleur pour les 5 catégories
palette <- brewer.pal(n = 5, name = "YlOrRd")


# Application de ce découpage à variable temps, sauvegarde dans temps_cat
# On stocke, pour chaque observation, la valeur de la couleur correspondante
comm@data$suffrage_cat <- as.character(cut(comm@data$X..Voix.Exp.1, breaks = classSuffrages$brks, labels = palette, include.lowest = TRUE))

# On stocke l'information des classes pour créer une légende 
legende <- as.character(levels(cut(comm@data$X..Voix.Exp.1, breaks = classSuffrages$brks, include.lowest = TRUE, right = FALSE)))

# Vérification
str(comm@data)
```

    ## 'data.frame':    1297 obs. of  167 variables:
    ##  $ ID_GEOFLA             : chr  "COMMUNE00000000000000017" "COMMUNE00000000000000063" "COMMUNE00000000000000083" "COMMUNE00000000000000085" ...
    ##  $ CODE_COM              : chr  "269" "333" "031" "523" ...
    ##  $ INSEE_COM             : chr  "78269" "77333" "77031" "77523" ...
    ##  $ NOM_COM               : chr  "GAZERAN" "NEMOURS" "BERNAY-VILBERT" "VILLUIS" ...
    ##  $ STATUT                : chr  "Commune simple" "Commune simple" "Commune simple" "Commune simple" ...
    ##  $ X_CHF_LIEU            : int  609631 677266 695422 726526 699484 659637 661284 678364 654494 659876 ...
    ##  $ Y_CHF_LIEU            : int  6837774 6796534 6841734 6812350 6802039 6876439 6834580 6866761 6877662 6805312 ...
    ##  $ X_CENTROID            : int  611101 678985 694725 726480 699173 660442 660868 677938 654924 659723 ...
    ##  $ Y_CENTROID            : int  6836525 6794893 6841708 6811782 6802390 6876741 6833535 6867276 6877366 6805473 ...
    ##  $ Z_MOYEN               : int  162 92 101 91 116 70 54 104 67 122 ...
    ##  $ SUPERFICIE            : num  2574 1009 1686 932 442 ...
    ##  $ POPULATION            : int  1272 12824 845 268 233 26075 47632 464 57533 368 ...
    ##  $ CODE_ARR              : chr  "2" "4" "3" "3" ...
    ##  $ CODE_DEPT             : chr  "78" "77" "77" "77" ...
    ##  $ NOM_DEPT              : chr  "YVELINES" "SEINE-ET-MARNE" "SEINE-ET-MARNE" "SEINE-ET-MARNE" ...
    ##  $ CODE_REG              : chr  "11" "11" "11" "11" ...
    ##  $ NOM_REG               : chr  "ILE-DE-FRANCE" "ILE-DE-FRANCE" "ILE-DE-FRANCE" "ILE-DE-FRANCE" ...
    ##  $ ZE2010                : int  1112 1107 1105 1108 1106 56 1115 56 56 1105 ...
    ##  $ Code.du.département   : chr  "78" "77" "77" "77" ...
    ##  $ Code.de.la.commune    : int  269 333 31 523 313 277 174 62 585 471 ...
    ##  $ Libellé.du.département: chr  "YVELINES" "SEINE ET MARNE" "SEINE ET MARNE" "SEINE ET MARNE" ...
    ##  $ Libellé.de.la.commune : chr  "Gazeran" "Nemours" "Bernay-Vilbert" "Villuis" ...
    ##  $ Inscrits              : int  923 6983 623 208 202 14127 22078 289 28210 299 ...
    ##  $ Abstentions           : int  136 1758 100 38 24 4005 6220 46 8236 58 ...
    ##  $ X..Abs.Ins            : num  14.7 25.2 16.1 18.3 11.9 ...
    ##  $ Votants               : int  787 5225 523 170 178 10122 15858 243 19974 241 ...
    ##  $ X..Vot.Ins            : num  85.3 74.8 84 81.7 88.1 ...
    ##  $ Blancs.et.nuls        : int  15 105 5 1 1 258 262 5 385 3 ...
    ##  $ X..BlNuls.Ins         : num  1.63 1.5 0.8 0.48 0.5 1.83 1.19 1.73 1.36 1 ...
    ##  $ X..BlNuls.Vot         : num  1.91 2.01 0.96 0.59 0.56 2.55 1.65 2.06 1.93 1.24 ...
    ##  $ Exprimés              : int  772 5120 518 169 177 9864 15596 238 19589 238 ...
    ##  $ X..Exp.Ins            : num  83.6 73.3 83.2 81.2 87.6 ...
    ##  $ X..Exp.Vot            : num  98.1 98 99 99.4 99.4 ...
    ##  $ Sexe                  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ Nom                   : chr  "JOLY" "JOLY" "JOLY" "JOLY" ...
    ##  $ Prénom                : chr  "Eva" "Eva" "Eva" "Eva" ...
    ##  $ Voix                  : int  35 87 9 3 5 131 357 11 214 6 ...
    ##  $ X..Voix.Ins           : num  3.79 1.25 1.44 1.44 2.48 0.93 1.62 3.81 0.76 2.01 ...
    ##  $ X..Voix.Exp           : num  4.53 1.7 1.74 1.78 2.82 1.33 2.29 4.62 1.09 2.52 ...
    ##  $ Sexe.1                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ Nom.1                 : chr  "LE PEN" "LE PEN" "LE PEN" "LE PEN" ...
    ##  $ Prénom.1              : chr  "Marine" "Marine" "Marine" "Marine" ...
    ##  $ Voix.1                : int  108 1026 96 50 39 1628 2625 41 1964 44 ...
    ##  $ X..Voix.Ins.1         : num  11.7 14.7 15.4 24 19.3 ...
    ##  $ X..Voix.Exp.1         : num  14 20 18.5 29.6 22 ...
    ##  $ Sexe.2                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.2                 : chr  "SARKOZY" "SARKOZY" "SARKOZY" "SARKOZY" ...
    ##  $ Prénom.2              : chr  "Nicolas" "Nicolas" "Nicolas" "Nicolas" ...
    ##  $ Voix.2                : int  222 1254 201 33 56 2336 2967 82 5247 77 ...
    ##  $ X..Voix.Ins.2         : num  24.1 18 32.3 15.9 27.7 ...
    ##  $ X..Voix.Exp.2         : num  28.8 24.5 38.8 19.5 31.6 ...
    ##  $ Sexe.3                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.3                 : chr  "MÉLENCHON" "MÉLENCHON" "MÉLENCHON" "MÉLENCHON" ...
    ##  $ Prénom.3              : chr  "Jean-Luc" "Jean-Luc" "Jean-Luc" "Jean-Luc" ...
    ##  $ Voix.3                : int  73 643 55 14 14 1140 2459 23 2236 30 ...
    ##  $ X..Voix.Ins.3         : num  7.91 9.21 8.83 6.73 6.93 ...
    ##  $ X..Voix.Exp.3         : num  9.46 12.56 10.62 8.28 7.91 ...
    ##  $ Sexe.4                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.4                 : chr  "POUTOU" "POUTOU" "POUTOU" "POUTOU" ...
    ##  $ Prénom.4              : chr  "Philippe" "Philippe" "Philippe" "Philippe" ...
    ##  $ Voix.4                : int  5 76 6 4 1 66 149 0 176 1 ...
    ##  $ X..Voix.Ins.4         : num  0.54 1.09 0.96 1.92 0.5 0.47 0.67 0 0.62 0.33 ...
    ##  $ X..Voix.Exp.4         : num  0.65 1.48 1.16 2.37 0.56 0.67 0.96 0 0.9 0.42 ...
    ##  $ Sexe.5                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ Nom.5                 : chr  "ARTHAUD" "ARTHAUD" "ARTHAUD" "ARTHAUD" ...
    ##  $ Prénom.5              : chr  "Nathalie" "Nathalie" "Nathalie" "Nathalie" ...
    ##  $ Voix.5                : int  3 33 2 0 0 42 106 0 71 1 ...
    ##  $ X..Voix.Ins.5         : num  0.33 0.47 0.32 0 0 0.3 0.48 0 0.25 0.33 ...
    ##  $ X..Voix.Exp.5         : num  0.39 0.64 0.39 0 0 0.43 0.68 0 0.36 0.42 ...
    ##  $ Sexe.6                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.6                 : chr  "CHEMINADE" "CHEMINADE" "CHEMINADE" "CHEMINADE" ...
    ##  $ Prénom.6              : chr  "Jacques" "Jacques" "Jacques" "Jacques" ...
    ##  $ Voix.6                : int  6 8 0 0 0 10 38 3 33 1 ...
    ##  $ X..Voix.Ins.6         : num  0.65 0.11 0 0 0 0.07 0.17 1.04 0.12 0.33 ...
    ##  $ X..Voix.Exp.6         : num  0.78 0.16 0 0 0 0.1 0.24 1.26 0.17 0.42 ...
    ##  $ Sexe.7                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.7                 : chr  "BAYROU" "BAYROU" "BAYROU" "BAYROU" ...
    ##  $ Prénom.7              : chr  "François" "François" "François" "François" ...
    ##  $ Voix.7                : int  103 351 35 18 16 477 1042 19 838 19 ...
    ##  $ X..Voix.Ins.7         : num  11.16 5.03 5.62 8.65 7.92 ...
    ##  $ X..Voix.Exp.7         : num  13.34 6.86 6.76 10.65 9.04 ...
    ##  $ Sexe.8                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.8                 : chr  "DUPONT-AIGNAN" "DUPONT-AIGNAN" "DUPONT-AIGNAN" "DUPONT-AIGNAN" ...
    ##  $ Prénom.8              : chr  "Nicolas" "Nicolas" "Nicolas" "Nicolas" ...
    ##  $ Voix.8                : int  18 102 21 8 2 121 292 3 200 3 ...
    ##  $ X..Voix.Ins.8         : num  1.95 1.46 3.37 3.85 0.99 0.86 1.32 1.04 0.71 1 ...
    ##  $ X..Voix.Exp.8         : num  2.33 1.99 4.05 4.73 1.13 1.23 1.87 1.26 1.02 1.26 ...
    ##  $ Sexe.9                : chr  "M" "M" "M" "M" ...
    ##  $ Nom.9                 : chr  "HOLLANDE" "HOLLANDE" "HOLLANDE" "HOLLANDE" ...
    ##  $ Prénom.9              : chr  "François" "François" "François" "François" ...
    ##  $ Voix.9                : int  199 1540 93 39 44 3913 5561 56 8610 56 ...
    ##  $ X..Voix.Ins.9         : num  21.6 22.1 14.9 18.8 21.8 ...
    ##  $ X..Voix.Exp.9         : num  25.8 30.1 17.9 23.1 24.9 ...
    ##  $ LIBGEO                : Factor w/ 33057 levels "Aast","Abainville",..: 11064 20446 2803 32413 19675 11453 7352 5107 28278 30299 ...
    ##  $ DEP                   : Factor w/ 101 levels "01","02","03",..: 79 78 78 78 78 96 92 78 96 78 ...
    ##  $ REG.x                 : int  11 11 11 11 11 11 11 11 11 11 ...
    ##  $ EPCI                  : Factor w/ 1268 levels "200000172","200000438",..: 717 35 701 161 1182 278 289 1185 278 684 ...
    ##  $ NATURE_EPCI           : Factor w/ 5 levels "CA","CC","CU",..: 1 2 2 2 2 1 1 1 1 1 ...
    ##  $ ARR                   : Factor w/ 334 levels "011","012","013",..: 271 268 267 267 267 320 308 269 320 268 ...
    ##   [list output truncated]

``` r
plot(comm, col = comm@data$suffrage_cat, border='black',lwd=0.001)
legend("bottomleft", legend = legende, fill = palette, cex=0.6, title = "Temps de parcours moyen (minutes)")
```

![](elections_france_files/figure-markdown_github/unnamed-chunk-21-1.png)
