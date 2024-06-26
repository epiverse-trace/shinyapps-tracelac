library(shiny)
library(epiCo)
library(incidence)
library(lubridate)
library(leaflet)
library(spdep)

library(shinyjs)
library(shinycssloaders)

ui <- fluidPage(
  # Application title
  titlePanel("epiCo"),
  sidebarLayout(
    sidebarPanel(
      width = 2,  # Ajustar el ancho
      fileInput("file", "Seleccione el archivo de incidentes", accept = ".csv"),
      uiOutput("yearSelector"),
      uiOutput('dropdown'),
      uiOutput('checkbox'),
      conditionalPanel(
        condition = "input.toggleSecondDropdown == true",
        uiOutput('second_dropdown')
      )
    ),
    
    mainPanel(
      shinyjs::useShinyjs(),
      h3(textOutput("selected_var")),
      fluidRow(
        column(6, plotOutput("populationPyramid") %>% withSpinner(color = "#FF0000")),
        column(6,
               conditionalPanel(
                 condition = "input.toggleSecondDropdown == true",
                 plotOutput("second_populationPyramid") %>% withSpinner(color = "#FF0000")
               )
        )
      ),
      
      
        conditionalPanel(
          condition = "output.plotLoaded == true",
          h3("Tasa de incidencia")
        ),
        fluidRow(
          column(6, plotOutput("incidenceRate") %>% withSpinner(color = "#FF0000")),
          column(6,
                 conditionalPanel(
                   condition = "input.toggleSecondDropdown == true",
                   plotOutput("second_incidenceRate") %>% withSpinner(color = "#FF0000")
                 )
          )
        ),
      
      
      conditionalPanel(
        condition = "output.plotLoaded == true",
        h3("Ocupaciones")
      ),
      fluidRow(
        column(6, plotOutput("occupationPlot") %>% withSpinner(color = "#FF0000")),
        column(6,
               conditionalPanel(
                 condition = "input.toggleSecondDropdown == true",
                 plotOutput("second_occupationPlot") %>% withSpinner(color = "#FF0000")
               )
        )
      ),
      
      conditionalPanel(
        condition = "output.plotLoaded == true",
        h3("Canal endémico")
      ),
      fluidRow(
        column(6, plotOutput("endemicChannel") %>% withSpinner(color = "#FF0000")),
        column(6,
               conditionalPanel(
                 condition = "input.toggleSecondDropdown == true",
                 plotOutput("second_endemicChannel") %>% withSpinner(color = "#FF0000")
               )
        )
      ),
      
      conditionalPanel(
        condition = "output.plotLoaded == true",
        h3("Índice de Moran")
      ),
      leafletOutput("moranIndex") %>% withSpinner(color = "#FF0000")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plotLoaded <- reactiveVal(FALSE)
  
  # Reactive expression to read the uploaded CSV file
  epidata <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
    epidata_file <<-read.csv(input$file$datapath)
  })
  
  # Lista de municipios con codigos
  placeList <- reactive({
    req(epidata())
    placeCodes <- as.numeric(unique(epidata()$cod_mun_o))
    allPlaces <- list(
      "MEDELLÍN" = 05001,
      "ABEJORRAL" = 05002,
      "ABRIAQUÍ" = 05004,
      "ALEJANDRÍA" = 05021,
      "AMAGÁ" = 05030,
      "AMALFI" = 05031,
      "ANDES" = 05034,
      "ANGELÓPOLIS" = 05036,
      "ANGOSTURA" = 05038,
      "ANORÍ" = 05040,
      "SANTAFÉ DE ANTIOQUIA" = 05042,
      "ANZA" = 05044,
      "APARTADÓ" = 05045,
      "ARBOLETES" = 05051,
      "ARGELIA" = 05055,
      "ARMENIA" = 05059,
      "BARBOSA" = 05079,
      "BELMIRA" = 05086,
      "BELLO" = 05088,
      "BETANIA" = 05091,
      "BETULIA" = 05093,
      "CIUDAD BOLÍVAR" = 05101,
      "BRICEÑO" = 05107,
      "BURITICÁ" = 05113,
      "CÁCERES" = 05120,
      "CAICEDO" = 05125,
      "CALDAS" = 05129,
      "CAMPAMENTO" = 05134,
      "CAÑASGORDAS" = 05138,
      "CARACOLÍ" = 05142,
      "CARAMANTA" = 05145,
      "CAREPA" = 05147,
      "EL CARMEN DE VIBORAL" = 05148,
      "CAROLINA" = 05150,
      "CAUCASIA" = 05154,
      "CHIGORODÓ" = 05172,
      "CISNEROS" = 05190,
      "COCORNÁ" = 05197,
      "CONCEPCIÓN" = 05206,
      "CONCORDIA" = 05209,
      "COPACABANA" = 05212,
      "DABEIBA" = 05234,
      "DON MATÍAS" = 05237,
      "EBÉJICO" = 05240,
      "EL BAGRE" = 05250,
      "ENTRERRIOS" = 05264,
      "ENVIGADO" = 05266,
      "FREDONIA" = 05282,
      "FRONTINO" = 05284,
      "GIRALDO" = 05306,
      "GIRARDOTA" = 05308,
      "GÓMEZ PLATA" = 05310,
      "GRANADA" = 05313,
      "GUADALUPE" = 05315,
      "GUARNE" = 05318,
      "GUATAPE" = 05321,
      "HELICONIA" = 05347,
      "HISPANIA" = 05353,
      "ITAGUI" = 05360,
      "ITUANGO" = 05361,
      "JARDÍN" = 05364,
      "JERICÓ" = 05368,
      "LA CEJA" = 05376,
      "LA ESTRELLA" = 05380,
      "LA PINTADA" = 05390,
      "LA UNIÓN" = 05400,
      "LIBORINA" = 05411,
      "MACEO" = 05425,
      "MARINILLA" = 05440,
      "MONTEBELLO" = 05467,
      "MURINDÓ" = 05475,
      "MUTATÁ" = 05480,
      "NARIÑO" = 05483,
      "NECOCLÍ" = 05490,
      "NECHÍ" = 05495,
      "OLAYA" = 05501,
      "PEÑOL" = 05541,
      "PEQUE" = 05543,
      "PUEBLORRICO" = 05576,
      "PUERTO BERRÍO" = 05579,
      "PUERTO NARE" = 05585,
      "PUERTO TRIUNFO" = 05591,
      "REMEDIOS" = 05604,
      "RETIRO" = 05607,
      "RIONEGRO" = 05615,
      "SABANALARGA" = 05628,
      "SABANETA" = 05631,
      "SALGAR" = 05642,
      "SAN ANDRÉS DE CUERQUÍA" = 05647,
      "SAN CARLOS" = 05649,
      "SAN FRANCISCO" = 05652,
      "SAN JERÓNIMO" = 05656,
      "SAN JOSÉ DE LA MONTAÑA" = 05658,
      "SAN JUAN DE URABÁ" = 05659,
      "SAN LUIS" = 05660,
      "SAN PEDRO" = 05664,
      "SAN PEDRO DE URABA" = 05665,
      "SAN RAFAEL" = 05667,
      "SAN ROQUE" = 05670,
      "SAN VICENTE" = 05674,
      "SANTA BÁRBARA" = 05679,
      "SANTA ROSA DE OSOS" = 05686,
      "SANTO DOMINGO" = 05690,
      "EL SANTUARIO" = 05697,
      "SEGOVIA" = 05736,
      "SONSON" = 05756,
      "SOPETRÁN" = 05761,
      "TÁMESIS" = 05789,
      "TARAZÁ" = 05790,
      "TARSO" = 05792,
      "TITIRIBÍ" = 05809,
      "TOLEDO" = 05819,
      "TURBO" = 05837,
      "URAMITA" = 05842,
      "URRAO" = 05847,
      "VALDIVIA" = 05854,
      "VALPARAÍSO" = 05856,
      "VEGACHÍ" = 05858,
      "VENECIA" = 05861,
      "VIGÍA DEL FUERTE" = 05873,
      "YALÍ" = 05885,
      "YARUMAL" = 05887,
      "YOLOMBÓ" = 05890,
      "YONDÓ" = 05893,
      "ZARAGOZA" = 05895,
      "BARRANQUILLA" = 08001,
      "BARANOA" = 08078,
      "CAMPO DE LA CRUZ" = 08137,
      "CANDELARIA" = 08141,
      "GALAPA" = 08296,
      "JUAN DE ACOSTA" = 08372,
      "LURUACO" = 08421,
      "MALAMBO" = 08433,
      "MANATÍ" = 08436,
      "PALMAR DE VARELA" = 08520,
      "PIOJÓ" = 08549,
      "POLONUEVO" = 08558,
      "PONEDERA" = 08560,
      "PUERTO COLOMBIA" = 08573,
      "REPELÓN" = 08606,
      "SABANAGRANDE" = 08634,
      "SABANALARGA" = 08638,
      "SANTA LUCÍA" = 08675,
      "SANTO TOMÁS" = 08685,
      "SOLEDAD" = 08758,
      "SUAN" = 08770,
      "TUBARÁ" = 08832,
      "USIACURÍ" = 08849,
      "BOGOTÁ, D.C." = 11001,
      "CARTAGENA" = 13001,
      "ACHÍ" = 13006,
      "ALTOS DEL ROSARIO" = 13030,
      "ARENAL" = 13042,
      "ARJONA" = 13052,
      "ARROYOHONDO" = 13062,
      "BARRANCO DE LOBA" = 13074,
      "CALAMAR" = 13140,
      "CANTAGALLO" = 13160,
      "CICUCO" = 13188,
      "CÓRDOBA" = 13212,
      "CLEMENCIA" = 13222,
      "EL CARMEN DE BOLÍVAR" = 13244,
      "EL GUAMO" = 13248,
      "EL PEÑÓN" = 13268,
      "HATILLO DE LOBA" = 13300,
      "MAGANGUÉ" = 13430,
      "MAHATES" = 13433,
      "MARGARITA" = 13440,
      "MARÍA LA BAJA" = 13442,
      "MONTECRISTO" = 13458,
      "MOMPÓS" = 13468,
      "MORALES" = 13473,
      "PINILLOS" = 13549,
      "REGIDOR" = 13580,
      "RÍO VIEJO" = 13600,
      "SAN CRISTÓBAL" = 13620,
      "SAN ESTANISLAO" = 13647,
      "SAN FERNANDO" = 13650,
      "SAN JACINTO" = 13654,
      "SAN JACINTO DEL CAUCA" = 13655,
      "SAN JUAN NEPOMUCENO" = 13657,
      "SAN MARTÍN DE LOBA" = 13667,
      "SAN PABLO" = 13670,
      "SANTA CATALINA" = 13673,
      "SANTA ROSA" = 13683,
      "SANTA ROSA DEL SUR" = 13688,
      "SIMITÍ" = 13744,
      "SOPLAVIENTO" = 13760,
      "TALAIGUA NUEVO" = 13780,
      "TIQUISIO" = 13810,
      "TURBACO" = 13836,
      "TURBANÁ" = 13838,
      "VILLANUEVA" = 13873,
      "ZAMBRANO" = 13894,
      "TUNJA" = 15001,
      "ALMEIDA" = 15022,
      "AQUITANIA" = 15047,
      "ARCABUCO" = 15051,
      "BELÉN" = 15087,
      "BERBEO" = 15090,
      "BETÉITIVA" = 15092,
      "BOAVITA" = 15097,
      "BOYACÁ" = 15104,
      "BRICEÑO" = 15106,
      "BUENAVISTA" = 15109,
      "BUSBANZÁ" = 15114,
      "CALDAS" = 15131,
      "CAMPOHERMOSO" = 15135,
      "CERINZA" = 15162,
      "CHINAVITA" = 15172,
      "CHIQUINQUIRÁ" = 15176,
      "CHISCAS" = 15180,
      "CHITA" = 15183,
      "CHITARAQUE" = 15185,
      "CHIVATÁ" = 15187,
      "CIÉNEGA" = 15189,
      "CÓMBITA" = 15204,
      "COPER" = 15212,
      "CORRALES" = 15215,
      "COVARACHÍA" = 15218,
      "CUBARÁ" = 15223,
      "CUCAITA" = 15224,
      "CUÍTIVA" = 15226,
      "CHÍQUIZA" = 15232,
      "CHIVOR" = 15236,
      "DUITAMA" = 15238,
      "EL COCUY" = 15244,
      "EL ESPINO" = 15248,
      "FIRAVITOBA" = 15272,
      "FLORESTA" = 15276,
      "GACHANTIVÁ" = 15293,
      "GAMEZA" = 15296,
      "GARAGOA" = 15299,
      "GUACAMAYAS" = 15317,
      "GUATEQUE" = 15322,
      "GUAYATÁ" = 15325,
      "GÜICÁN" = 15332,
      "IZA" = 15362,
      "JENESANO" = 15367,
      "JERICÓ" = 15368,
      "LABRANZAGRANDE" = 15377,
      "LA CAPILLA" = 15380,
      "LA VICTORIA" = 15401,
      "LA UVITA" = 15403,
      "VILLA DE LEYVA" = 15407,
      "MACANAL" = 15425,
      "MARIPÍ" = 15442,
      "MIRAFLORES" = 15455,
      "MONGUA" = 15464,
      "MONGUÍ" = 15466,
      "MONIQUIRÁ" = 15469,
      "MOTAVITA" = 15476,
      "MUZO" = 15480,
      "NOBSA" = 15491,
      "NUEVO COLÓN" = 15494,
      "OICATÁ" = 15500,
      "OTANCHE" = 15507,
      "PACHAVITA" = 15511,
      "PÁEZ" = 15514,
      "PAIPA" = 15516,
      "PAJARITO" = 15518,
      "PANQUEBA" = 15522,
      "PAUNA" = 15531,
      "PAYA" = 15533,
      "PAZ DE RÍO" = 15537,
      "PESCA" = 15542,
      "PISBA" = 15550,
      "PUERTO BOYACÁ" = 15572,
      "QUÍPAMA" = 15580,
      "RAMIRIQUÍ" = 15599,
      "RÁQUIRA" = 15600,
      "RONDÓN" = 15621,
      "SABOYÁ" = 15632,
      "SÁCHICA" = 15638,
      "SAMACÁ" = 15646,
      "SAN EDUARDO" = 15660,
      "SAN JOSÉ DE PARE" = 15664,
      "SAN LUIS DE GACENO" = 15667,
      "SAN MATEO" = 15673,
      "SAN MIGUEL DE SEMA" = 15676,
      "SAN PABLO DE BORBUR" = 15681,
      "SANTANA" = 15686,
      "SANTA MARÍA" = 15690,
      "SANTA ROSA DE VITERBO" = 15693,
      "SANTA SOFÍA" = 15696,
      "SATIVANORTE" = 15720,
      "SATIVASUR" = 15723,
      "SIACHOQUE" = 15740,
      "SOATÁ" = 15753,
      "SOCOTÁ" = 15755,
      "SOCHA" = 15757,
      "SOGAMOSO" = 15759,
      "SOMONDOCO" = 15761,
      "SORA" = 15762,
      "SOTAQUIRÁ" = 15763,
      "SORACÁ" = 15764,
      "SUSACÓN" = 15774,
      "SUTAMARCHÁN" = 15776,
      "SUTATENZA" = 15778,
      "TASCO" = 15790,
      "TENZA" = 15798,
      "TIBANÁ" = 15804,
      "TIBASOSA" = 15806,
      "TINJACÁ" = 15808,
      "TIPACOQUE" = 15810,
      "TOCA" = 15814,
      "TOGÜÍ" = 15816,
      "TÓPAGA" = 15820,
      "TOTA" = 15822,
      "TUNUNGUÁ" = 15832,
      "TURMEQUÉ" = 15835,
      "TUTA" = 15837,
      "TUTAZÁ" = 15839,
      "UMBITA" = 15842,
      "VENTAQUEMADA" = 15861,
      "VIRACACHÁ" = 15879,
      "ZETAQUIRA" = 15897,
      "MANIZALES" = 17001,
      "AGUADAS" = 17013,
      "ANSERMA" = 17042,
      "ARANZAZU" = 17050,
      "BELALCÁZAR" = 17088,
      "CHINCHINÁ" = 17174,
      "FILADELFIA" = 17272,
      "LA DORADA" = 17380,
      "LA MERCED" = 17388,
      "MANZANARES" = 17433,
      "MARMATO" = 17442,
      "MARQUETALIA" = 17444,
      "MARULANDA" = 17446,
      "NEIRA" = 17486,
      "NORCASIA" = 17495,
      "PÁCORA" = 17513,
      "PALESTINA" = 17524,
      "PENSILVANIA" = 17541,
      "RIOSUCIO" = 17614,
      "RISARALDA" = 17616,
      "SALAMINA" = 17653,
      "SAMANÁ" = 17662,
      "SAN JOSÉ" = 17665,
      "SUPÍA" = 17777,
      "VICTORIA" = 17867,
      "VILLAMARÍA" = 17873,
      "VITERBO" = 17877,
      "FLORENCIA" = 18001,
      "ALBANIA" = 18029,
      "BELÉN DE LOS ANDAQUIES" = 18094,
      "CARTAGENA DEL CHAIRÁ" = 18150,
      "CURILLO" = 18205,
      "EL DONCELLO" = 18247,
      "EL PAUJIL" = 18256,
      "LA MONTAÑITA" = 18410,
      "MILÁN" = 18460,
      "MORELIA" = 18479,
      "PUERTO RICO" = 18592,
      "SAN JOSÉ DEL FRAGUA" = 18610,
      "SAN VICENTE DEL CAGUÁN" = 18753,
      "SOLANO" = 18756,
      "SOLITA" = 18785,
      "VALPARAÍSO" = 18860,
      "POPAYÁN" = 19001,
      "ALMAGUER" = 19022,
      "ARGELIA" = 19050,
      "BALBOA" = 19075,
      "BOLÍVAR" = 19100,
      "BUENOS AIRES" = 19110,
      "CAJIBÍO" = 19130,
      "CALDONO" = 19137,
      "CALOTO" = 19142,
      "CORINTO" = 19212,
      "EL TAMBO" = 19256,
      "FLORENCIA" = 19290,
      "GUACHENÉ" = 19300,
      "GUAPI" = 19318,
      "INZÁ" = 19355,
      "JAMBALÓ" = 19364,
      "LA SIERRA" = 19392,
      "LA VEGA" = 19397,
      "LÓPEZ" = 19418,
      "MERCADERES" = 19450,
      "MIRANDA" = 19455,
      "MORALES" = 19473,
      "PADILLA" = 19513,
      "PAEZ" = 19517,
      "PATÍA" = 19532,
      "PIAMONTE" = 19533,
      "PIENDAMÓ" = 19548,
      "PUERTO TEJADA" = 19573,
      "PURACÉ" = 19585,
      "ROSAS" = 19622,
      "SAN SEBASTIÁN" = 19693,
      "SANTANDER DE QUILICHAO" = 19698,
      "SANTA ROSA" = 19701,
      "SILVIA" = 19743,
      "SOTARA" = 19760,
      "SUÁREZ" = 19780,
      "SUCRE" = 19785,
      "TIMBÍO" = 19807,
      "TIMBIQUÍ" = 19809,
      "TORIBIO" = 19821,
      "TOTORÓ" = 19824,
      "VILLA RICA" = 19845,
      "VALLEDUPAR" = 20001,
      "AGUACHICA" = 20011,
      "AGUSTÍN CODAZZI" = 20013,
      "ASTREA" = 20032,
      "BECERRIL" = 20045,
      "BOSCONIA" = 20060,
      "CHIMICHAGUA" = 20175,
      "CHIRIGUANÁ" = 20178,
      "CURUMANÍ" = 20228,
      "EL COPEY" = 20238,
      "EL PASO" = 20250,
      "GAMARRA" = 20295,
      "GONZÁLEZ" = 20310,
      "LA GLORIA" = 20383,
      "LA JAGUA DE IBIRICO" = 20400,
      "MANAURE" = 20443,
      "PAILITAS" = 20517,
      "PELAYA" = 20550,
      "PUEBLO BELLO" = 20570,
      "RÍO DE ORO" = 20614,
      "LA PAZ" = 20621,
      "SAN ALBERTO" = 20710,
      "SAN DIEGO" = 20750,
      "SAN MARTÍN" = 20770,
      "TAMALAMEQUE" = 20787,
      "MONTERÍA" = 23001,
      "AYAPEL" = 23068,
      "BUENAVISTA" = 23079,
      "CANALETE" = 23090,
      "CERETÉ" = 23162,
      "CHIMÁ" = 23168,
      "CHINÚ" = 23182,
      "CIÉNAGA DE ORO" = 23189,
      "COTORRA" = 23300,
      "LA APARTADA" = 23350,
      "LORICA" = 23417,
      "LOS CÓRDOBAS" = 23419,
      "MOMIL" = 23464,
      "MONTELÍBANO" = 23466,
      "MOÑITOS" = 23500,
      "PLANETA RICA" = 23555,
      "PUEBLO NUEVO" = 23570,
      "PUERTO ESCONDIDO" = 23574,
      "PUERTO LIBERTADOR" = 23580,
      "PURÍSIMA" = 23586,
      "SAHAGÚN" = 23660,
      "SAN ANDRÉS SOTAVENTO" = 23670,
      "SAN ANTERO" = 23672,
      "SAN BERNARDO DEL VIENTO" = 23675,
      "SAN CARLOS" = 23678,
      "SAN PELAYO" = 23686,
      "TIERRALTA" = 23807,
      "VALENCIA" = 23855,
      "AGUA DE DIOS" = 25001,
      "ALBÁN" = 25019,
      "ANAPOIMA" = 25035,
      "ANOLAIMA" = 25040,
      "ARBELÁEZ" = 25053,
      "BELTRÁN" = 25086,
      "BITUIMA" = 25095,
      "BOJACÁ" = 25099,
      "CABRERA" = 25120,
      "CACHIPAY" = 25123,
      "CAJICÁ" = 25126,
      "CAPARRAPÍ" = 25148,
      "CAQUEZA" = 25151,
      "CARMEN DE CARUPA" = 25154,
      "CHAGUANÍ" = 25168,
      "CHÍA" = 25175,
      "CHIPAQUE" = 25178,
      "CHOACHÍ" = 25181,
      "CHOCONTÁ" = 25183,
      "COGUA" = 25200,
      "COTA" = 25214,
      "CUCUNUBÁ" = 25224,
      "EL COLEGIO" = 25245,
      "EL PEÑÓN" = 25258,
      "EL ROSAL" = 25260,
      "FACATATIVÁ" = 25269,
      "FOMEQUE" = 25279,
      "FOSCA" = 25281,
      "FUNZA" = 25286,
      "FÚQUENE" = 25288,
      "FUSAGASUGÁ" = 25290,
      "GACHALA" = 25293,
      "GACHANCIPÁ" = 25295,
      "GACHETÁ" = 25297,
      "GAMA" = 25299,
      "GIRARDOT" = 25307,
      "GRANADA" = 25312,
      "GUACHETÁ" = 25317,
      "GUADUAS" = 25320,
      "GUASCA" = 25322,
      "GUATAQUÍ" = 25324,
      "GUATAVITA" = 25326,
      "GUAYABAL DE SIQUIMA" = 25328,
      "GUAYABETAL" = 25335,
      "GUTIÉRREZ" = 25339,
      "JERUSALÉN" = 25368,
      "JUNÍN" = 25372,
      "LA CALERA" = 25377,
      "LA MESA" = 25386,
      "LA PALMA" = 25394,
      "LA PEÑA" = 25398,
      "LA VEGA" = 25402,
      "LENGUAZAQUE" = 25407,
      "MACHETA" = 25426,
      "MADRID" = 25430,
      "MANTA" = 25436,
      "MEDINA" = 25438,
      "MOSQUERA" = 25473,
      "NARIÑO" = 25483,
      "NEMOCÓN" = 25486,
      "NILO" = 25488,
      "NIMAIMA" = 25489,
      "NOCAIMA" = 25491,
      "VENECIA" = 25506,
      "PACHO" = 25513,
      "PAIME" = 25518,
      "PANDI" = 25524,
      "PARATEBUENO" = 25530,
      "PASCA" = 25535,
      "PUERTO SALGAR" = 25572,
      "PULÍ" = 25580,
      "QUEBRADANEGRA" = 25592,
      "QUETAME" = 25594,
      "QUIPILE" = 25596,
      "APULO" = 25599,
      "RICAURTE" = 25612,
      "SAN ANTONIO DEL TEQUENDAMA" = 25645,
      "SAN BERNARDO" = 25649,
      "SAN CAYETANO" = 25653,
      "SAN FRANCISCO" = 25658,
      "SAN JUAN DE RÍO SECO" = 25662,
      "SASAIMA" = 25718,
      "SESQUILÉ" = 25736,
      "SIBATÉ" = 25740,
      "SILVANIA" = 25743,
      "SIMIJACA" = 25745,
      "SOACHA" = 25754,
      "SOPÓ" = 25758,
      "SUBACHOQUE" = 25769,
      "SUESCA" = 25772,
      "SUPATÁ" = 25777,
      "SUSA" = 25779,
      "SUTATAUSA" = 25781,
      "TABIO" = 25785,
      "TAUSA" = 25793,
      "TENA" = 25797,
      "TENJO" = 25799,
      "TIBACUY" = 25805,
      "TIBIRITA" = 25807,
      "TOCAIMA" = 25815,
      "TOCANCIPÁ" = 25817,
      "TOPAIPÍ" = 25823,
      "UBALÁ" = 25839,
      "UBAQUE" = 25841,
      "VILLA DE SAN DIEGO DE UBATE" = 25843,
      "UNE" = 25845,
      "ÚTICA" = 25851,
      "VERGARA" = 25862,
      "VIANÍ" = 25867,
      "VILLAGÓMEZ" = 25871,
      "VILLAPINZÓN" = 25873,
      "VILLETA" = 25875,
      "VIOTÁ" = 25878,
      "YACOPÍ" = 25885,
      "ZIPACÓN" = 25898,
      "ZIPAQUIRÁ" = 25899,
      "QUIBDÓ" = 27001,
      "ACANDÍ" = 27006,
      "ALTO BAUDO" = 27025,
      "ATRATO" = 27050,
      "BAGADÓ" = 27073,
      "BAHÍA SOLANO" = 27075,
      "BAJO BAUDÓ" = 27077,
      "BELÉN DE BAJIRÁ" = 27086,
      "BOJAYA" = 27099,
      "EL CANTÓN DEL SAN PABLO" = 27135,
      "CARMEN DEL DARIEN" = 27150,
      "CÉRTEGUI" = 27160,
      "CONDOTO" = 27205,
      "EL CARMEN DE ATRATO" = 27245,
      "EL LITORAL DEL SAN JUAN" = 27250,
      "ISTMINA" = 27361,
      "JURADÓ" = 27372,
      "LLORÓ" = 27413,
      "MEDIO ATRATO" = 27425,
      "MEDIO BAUDÓ" = 27430,
      "MEDIO SAN JUAN" = 27450,
      "NÓVITA" = 27491,
      "NUQUÍ" = 27495,
      "RÍO IRO" = 27580,
      "RÍO QUITO" = 27600,
      "RIOSUCIO" = 27615,
      "SAN JOSÉ DEL PALMAR" = 27660,
      "SIPÍ" = 27745,
      "TADÓ" = 27787,
      "UNGUÍA" = 27800,
      "UNIÓN PANAMERICANA" = 27810,
      "NEIVA" = 41001,
      "ACEVEDO" = 41006,
      "AGRADO" = 41013,
      "AIPE" = 41016,
      "ALGECIRAS" = 41020,
      "ALTAMIRA" = 41026,
      "BARAYA" = 41078,
      "CAMPOALEGRE" = 41132,
      "COLOMBIA" = 41206,
      "ELÍAS" = 41244,
      "GARZÓN" = 41298,
      "GIGANTE" = 41306,
      "GUADALUPE" = 41319,
      "HOBO" = 41349,
      "IQUIRA" = 41357,
      "ISNOS" = 41359,
      "LA ARGENTINA" = 41378,
      "LA PLATA" = 41396,
      "NÁTAGA" = 41483,
      "OPORAPA" = 41503,
      "PAICOL" = 41518,
      "PALERMO" = 41524,
      "PALESTINA" = 41530,
      "PITAL" = 41548,
      "PITALITO" = 41551,
      "RIVERA" = 41615,
      "SALADOBLANCO" = 41660,
      "SAN AGUSTÍN" = 41668,
      "SANTA MARÍA" = 41676,
      "SUAZA" = 41770,
      "TARQUI" = 41791,
      "TESALIA" = 41797,
      "TELLO" = 41799,
      "TERUEL" = 41801,
      "TIMANÁ" = 41807,
      "VILLAVIEJA" = 41872,
      "YAGUARÁ" = 41885,
      "RIOHACHA" = 44001,
      "ALBANIA" = 44035,
      "BARRANCAS" = 44078,
      "DIBULLA" = 44090,
      "DISTRACCIÓN" = 44098,
      "EL MOLINO" = 44110,
      "FONSECA" = 44279,
      "HATONUEVO" = 44378,
      "LA JAGUA DEL PILAR" = 44420,
      "MAICAO" = 44430,
      "MANAURE" = 44560,
      "SAN JUAN DEL CESAR" = 44650,
      "URIBIA" = 44847,
      "URUMITA" = 44855,
      "VILLANUEVA" = 44874,
      "SANTA MARTA" = 47001,
      "ALGARROBO" = 47030,
      "ARACATACA" = 47053,
      "ARIGUANÍ" = 47058,
      "CERRO SAN ANTONIO" = 47161,
      "CHIBOLO" = 47170,
      "CIÉNAGA" = 47189,
      "CONCORDIA" = 47205,
      "EL BANCO" = 47245,
      "EL PIÑON" = 47258,
      "EL RETÉN" = 47268,
      "FUNDACIÓN" = 47288,
      "GUAMAL" = 47318,
      "NUEVA GRANADA" = 47460,
      "PEDRAZA" = 47541,
      "PIJIÑO DEL CARMEN" = 47545,
      "PIVIJAY" = 47551,
      "PLATO" = 47555,
      "PUEBLOVIEJO" = 47570,
      "REMOLINO" = 47605,
      "SABANAS DE SAN ANGEL" = 47660,
      "SALAMINA" = 47675,
      "SAN SEBASTIÁN DE BUENAVISTA" = 47692,
      "SAN ZENÓN" = 47703,
      "SANTA ANA" = 47707,
      "SANTA BÁRBARA DE PINTO" = 47720,
      "SITIONUEVO" = 47745,
      "TENERIFE" = 47798,
      "ZAPAYÁN" = 47960,
      "ZONA BANANERA" = 47980,
      "VILLAVICENCIO" = 50001,
      "ACACÍAS" = 50006,
      "BARRANCA DE UPÍA" = 50110,
      "CABUYARO" = 50124,
      "CASTILLA LA NUEVA" = 50150,
      "CUBARRAL" = 50223,
      "CUMARAL" = 50226,
      "EL CALVARIO" = 50245,
      "EL CASTILLO" = 50251,
      "EL DORADO" = 50270,
      "FUENTE DE ORO" = 50287,
      "GRANADA" = 50313,
      "GUAMAL" = 50318,
      "MAPIRIPÁN" = 50325,
      "MESETAS" = 50330,
      "LA MACARENA" = 50350,
      "URIBE" = 50370,
      "LEJANÍAS" = 50400,
      "PUERTO CONCORDIA" = 50450,
      "PUERTO GAITÁN" = 50568,
      "PUERTO LÓPEZ" = 50573,
      "PUERTO LLERAS" = 50577,
      "PUERTO RICO" = 50590,
      "RESTREPO" = 50606,
      "SAN CARLOS DE GUAROA" = 50680,
      "SAN JUAN DE ARAMA" = 50683,
      "SAN JUANITO" = 50686,
      "SAN MARTÍN" = 50689,
      "VISTAHERMOSA" = 50711,
      "PASTO" = 52001,
      "ALBÁN" = 52019,
      "ALDANA" = 52022,
      "ANCUYÁ" = 52036,
      "ARBOLEDA" = 52051,
      "BARBACOAS" = 52079,
      "BELÉN" = 52083,
      "BUESACO" = 52110,
      "COLÓN" = 52203,
      "CONSACA" = 52207,
      "CONTADERO" = 52210,
      "CÓRDOBA" = 52215,
      "CUASPUD" = 52224,
      "CUMBAL" = 52227,
      "CUMBITARA" = 52233,
      "CHACHAGÜÍ" = 52240,
      "EL CHARCO" = 52250,
      "EL PEÑOL" = 52254,
      "EL ROSARIO" = 52256,
      "EL TABLÓN DE GÓMEZ" = 52258,
      "EL TAMBO" = 52260,
      "FUNES" = 52287,
      "GUACHUCAL" = 52317,
      "GUAITARILLA" = 52320,
      "GUALMATÁN" = 52323,
      "ILES" = 52352,
      "IMUÉS" = 52354,
      "IPIALES" = 52356,
      "LA CRUZ" = 52378,
      "LA FLORIDA" = 52381,
      "LA LLANADA" = 52385,
      "LA TOLA" = 52390,
      "LA UNIÓN" = 52399,
      "LEIVA" = 52405,
      "LINARES" = 52411,
      "LOS ANDES" = 52418,
      "MAGÜI" = 52427,
      "MALLAMA" = 52435,
      "MOSQUERA" = 52473,
      "NARIÑO" = 52480,
      "OLAYA HERRERA" = 52490,
      "OSPINA" = 52506,
      "FRANCISCO PIZARRO" = 52520,
      "POLICARPA" = 52540,
      "POTOSÍ" = 52560,
      "PROVIDENCIA" = 52565,
      "PUERRES" = 52573,
      "PUPIALES" = 52585,
      "RICAURTE" = 52612,
      "ROBERTO PAYÁN" = 52621,
      "SAMANIEGO" = 52678,
      "SANDONÁ" = 52683,
      "SAN BERNARDO" = 52685,
      "SAN LORENZO" = 52687,
      "SAN PABLO" = 52693,
      "SAN PEDRO DE CARTAGO" = 52694,
      "SANTA BÁRBARA" = 52696,
      "SANTACRUZ" = 52699,
      "SAPUYES" = 52720,
      "TAMINANGO" = 52786,
      "TANGUA" = 52788,
      "SAN ANDRES DE TUMACO" = 52835,
      "TÚQUERRES" = 52838,
      "YACUANQUER" = 52885,
      "CÚCUTA" = 54001,
      "ABREGO" = 54003,
      "ARBOLEDAS" = 54051,
      "BOCHALEMA" = 54099,
      "BUCARASICA" = 54109,
      "CÁCOTA" = 54125,
      "CACHIRÁ" = 54128,
      "CHINÁCOTA" = 54172,
      "CHITAGÁ" = 54174,
      "CONVENCIÓN" = 54206,
      "CUCUTILLA" = 54223,
      "DURANIA" = 54239,
      "EL CARMEN" = 54245,
      "EL TARRA" = 54250,
      "EL ZULIA" = 54261,
      "GRAMALOTE" = 54313,
      "HACARÍ" = 54344,
      "HERRÁN" = 54347,
      "LABATECA" = 54377,
      "LA ESPERANZA" = 54385,
      "LA PLAYA" = 54398,
      "LOS PATIOS" = 54405,
      "LOURDES" = 54418,
      "MUTISCUA" = 54480,
      "OCAÑA" = 54498,
      "PAMPLONA" = 54518,
      "PAMPLONITA" = 54520,
      "PUERTO SANTANDER" = 54553,
      "RAGONVALIA" = 54599,
      "SALAZAR" = 54660,
      "SAN CALIXTO" = 54670,
      "SAN CAYETANO" = 54673,
      "SANTIAGO" = 54680,
      "SARDINATA" = 54720,
      "SILOS" = 54743,
      "TEORAMA" = 54800,
      "TIBÚ" = 54810,
      "TOLEDO" = 54820,
      "VILLA CARO" = 54871,
      "VILLA DEL ROSARIO" = 54874,
      "ARMENIA" = 63001,
      "BUENAVISTA" = 63111,
      "CALARCA" = 63130,
      "CIRCASIA" = 63190,
      "CÓRDOBA" = 63212,
      "FILANDIA" = 63272,
      "GÉNOVA" = 63302,
      "LA TEBAIDA" = 63401,
      "MONTENEGRO" = 63470,
      "PIJAO" = 63548,
      "QUIMBAYA" = 63594,
      "SALENTO" = 63690,
      "PEREIRA" = 66001,
      "APÍA" = 66045,
      "BALBOA" = 66075,
      "BELÉN DE UMBRÍA" = 66088,
      "DOSQUEBRADAS" = 66170,
      "GUÁTICA" = 66318,
      "LA CELIA" = 66383,
      "LA VIRGINIA" = 66400,
      "MARSELLA" = 66440,
      "MISTRATÓ" = 66456,
      "PUEBLO RICO" = 66572,
      "QUINCHÍA" = 66594,
      "SANTA ROSA DE CABAL" = 66682,
      "SANTUARIO" = 66687,
      "BUCARAMANGA" = 68001,
      "AGUADA" = 68013,
      "ALBANIA" = 68020,
      "ARATOCA" = 68051,
      "BARBOSA" = 68077,
      "BARICHARA" = 68079,
      "BARRANCABERMEJA" = 68081,
      "BETULIA" = 68092,
      "BOLÍVAR" = 68101,
      "CABRERA" = 68121,
      "CALIFORNIA" = 68132,
      "CAPITANEJO" = 68147,
      "CARCASÍ" = 68152,
      "CEPITÁ" = 68160,
      "CERRITO" = 68162,
      "CHARALÁ" = 68167,
      "CHARTA" = 68169,
      "CHIMA" = 68176,
      "CHIPATÁ" = 68179,
      "CIMITARRA" = 68190,
      "CONCEPCIÓN" = 68207,
      "CONFINES" = 68209,
      "CONTRATACIÓN" = 68211,
      "COROMORO" = 68217,
      "CURITÍ" = 68229,
      "EL CARMEN DE CHUCURÍ" = 68235,
      "EL GUACAMAYO" = 68245,
      "EL PEÑÓN" = 68250,
      "EL PLAYÓN" = 68255,
      "ENCINO" = 68264,
      "ENCISO" = 68266,
      "FLORIÁN" = 68271,
      "FLORIDABLANCA" = 68276,
      "GALÁN" = 68296,
      "GAMBITA" = 68298,
      "GIRÓN" = 68307,
      "GUACA" = 68318,
      "GUADALUPE" = 68320,
      "GUAPOTÁ" = 68322,
      "GUAVATÁ" = 68324,
      "GÜEPSA" = 68327,
      "HATO" = 68344,
      "JESÚS MARÍA" = 68368,
      "JORDÁN" = 68370,
      "LA BELLEZA" = 68377,
      "LANDÁZURI" = 68385,
      "LA PAZ" = 68397,
      "LEBRÍJA" = 68406,
      "LOS SANTOS" = 68418,
      "MACARAVITA" = 68425,
      "MÁLAGA" = 68432,
      "MATANZA" = 68444,
      "MOGOTES" = 68464,
      "MOLAGAVITA" = 68468,
      "OCAMONTE" = 68498,
      "OIBA" = 68500,
      "ONZAGA" = 68502,
      "PALMAR" = 68522,
      "PALMAS DEL SOCORRO" = 68524,
      "PÁRAMO" = 68533,
      "PIEDECUESTA" = 68547,
      "PINCHOTE" = 68549,
      "PUENTE NACIONAL" = 68572,
      "PUERTO PARRA" = 68573,
      "PUERTO WILCHES" = 68575,
      "RIONEGRO" = 68615,
      "SABANA DE TORRES" = 68655,
      "SAN ANDRÉS" = 68669,
      "SAN BENITO" = 68673,
      "SAN GIL" = 68679,
      "SAN JOAQUÍN" = 68682,
      "SAN JOSÉ DE MIRANDA" = 68684,
      "SAN MIGUEL" = 68686,
      "SAN VICENTE DE CHUCURÍ" = 68689,
      "SANTA BÁRBARA" = 68705,
      "SANTA HELENA DEL OPÓN" = 68720,
      "SIMACOTA" = 68745,
      "SOCORRO" = 68755,
      "SUAITA" = 68770,
      "SUCRE" = 68773,
      "SURATÁ" = 68780,
      "TONA" = 68820,
      "VALLE DE SAN JOSÉ" = 68855,
      "VÉLEZ" = 68861,
      "VETAS" = 68867,
      "VILLANUEVA" = 68872,
      "ZAPATOCA" = 68895,
      "SINCELEJO" = 70001,
      "BUENAVISTA" = 70110,
      "CAIMITO" = 70124,
      "COLOSO" = 70204,
      "COROZAL" = 70215,
      "COVEÑAS" = 70221,
      "CHALÁN" = 70230,
      "EL ROBLE" = 70233,
      "GALERAS" = 70235,
      "GUARANDA" = 70265,
      "LA UNIÓN" = 70400,
      "LOS PALMITOS" = 70418,
      "MAJAGUAL" = 70429,
      "MORROA" = 70473,
      "OVEJAS" = 70508,
      "PALMITO" = 70523,
      "SAMPUÉS" = 70670,
      "SAN BENITO ABAD" = 70678,
      "SAN JUAN DE BETULIA" = 70702,
      "SAN MARCOS" = 70708,
      "SAN ONOFRE" = 70713,
      "SAN PEDRO" = 70717,
      "SAN LUIS DE SINCÉ" = 70742,
      "SUCRE" = 70771,
      "SANTIAGO DE TOLÚ" = 70820,
      "TOLÚ VIEJO" = 70823,
      "IBAGUÉ" = 73001,
      "ALPUJARRA" = 73024,
      "ALVARADO" = 73026,
      "AMBALEMA" = 73030,
      "ANZOÁTEGUI" = 73043,
      "ARMERO" = 73055,
      "ATACO" = 73067,
      "CAJAMARCA" = 73124,
      "CARMEN DE APICALÁ" = 73148,
      "CASABIANCA" = 73152,
      "CHAPARRAL" = 73168,
      "COELLO" = 73200,
      "COYAIMA" = 73217,
      "CUNDAY" = 73226,
      "DOLORES" = 73236,
      "ESPINAL" = 73268,
      "FALAN" = 73270,
      "FLANDES" = 73275,
      "FRESNO" = 73283,
      "GUAMO" = 73319,
      "HERVEO" = 73347,
      "HONDA" = 73349,
      "ICONONZO" = 73352,
      "LÉRIDA" = 73408,
      "LÍBANO" = 73411,
      "MARIQUITA" = 73443,
      "MELGAR" = 73449,
      "MURILLO" = 73461,
      "NATAGAIMA" = 73483,
      "ORTEGA" = 73504,
      "PALOCABILDO" = 73520,
      "PIEDRAS" = 73547,
      "PLANADAS" = 73555,
      "PRADO" = 73563,
      "PURIFICACIÓN" = 73585,
      "RIOBLANCO" = 73616,
      "RONCESVALLES" = 73622,
      "ROVIRA" = 73624,
      "SALDAÑA" = 73671,
      "SAN ANTONIO" = 73675,
      "SAN LUIS" = 73678,
      "SANTA ISABEL" = 73686,
      "SUÁREZ" = 73770,
      "VALLE DE SAN JUAN" = 73854,
      "VENADILLO" = 73861,
      "VILLAHERMOSA" = 73870,
      "VILLARRICA" = 73873,
      "CALI" = 76001,
      "ALCALÁ" = 76020,
      "ANDALUCÍA" = 76036,
      "ANSERMANUEVO" = 76041,
      "ARGELIA" = 76054,
      "BOLÍVAR" = 76100,
      "BUENAVENTURA" = 76109,
      "GUADALAJARA DE BUGA" = 76111,
      "BUGALAGRANDE" = 76113,
      "CAICEDONIA" = 76122,
      "CALIMA" = 76126,
      "CANDELARIA" = 76130,
      "CARTAGO" = 76147,
      "DAGUA" = 76233,
      "EL ÁGUILA" = 76243,
      "EL CAIRO" = 76246,
      "EL CERRITO" = 76248,
      "EL DOVIO" = 76250,
      "FLORIDA" = 76275,
      "GINEBRA" = 76306,
      "GUACARÍ" = 76318,
      "JAMUNDÍ" = 76364,
      "LA CUMBRE" = 76377,
      "LA UNIÓN" = 76400,
      "LA VICTORIA" = 76403,
      "OBANDO" = 76497,
      "PALMIRA" = 76520,
      "PRADERA" = 76563,
      "RESTREPO" = 76606,
      "RIOFRÍO" = 76616,
      "ROLDANILLO" = 76622,
      "SAN PEDRO" = 76670,
      "SEVILLA" = 76736,
      "TORO" = 76823,
      "TRUJILLO" = 76828,
      "TULUÁ" = 76834,
      "ULLOA" = 76845,
      "VERSALLES" = 76863,
      "VIJES" = 76869,
      "YOTOCO" = 76890,
      "YUMBO" = 76892,
      "ZARZAL" = 76895,
      "ARAUCA" = 81001,
      "ARAUQUITA" = 81065,
      "CRAVO NORTE" = 81220,
      "FORTUL" = 81300,
      "PUERTO RONDÓN" = 81591,
      "SARAVENA" = 81736,
      "TAME" = 81794,
      "YOPAL" = 85001,
      "AGUAZUL" = 85010,
      "CHAMEZA" = 85015,
      "HATO COROZAL" = 85125,
      "LA SALINA" = 85136,
      "MANÍ" = 85139,
      "MONTERREY" = 85162,
      "NUNCHÍA" = 85225,
      "OROCUÉ" = 85230,
      "PAZ DE ARIPORO" = 85250,
      "PORE" = 85263,
      "RECETOR" = 85279,
      "SABANALARGA" = 85300,
      "SÁCAMA" = 85315,
      "SAN LUIS DE PALENQUE" = 85325,
      "TÁMARA" = 85400,
      "TAURAMENA" = 85410,
      "TRINIDAD" = 85430,
      "VILLANUEVA" = 85440,
      "MOCOA" = 86001,
      "COLÓN" = 86219,
      "ORITO" = 86320,
      "PUERTO ASÍS" = 86568,
      "PUERTO CAICEDO" = 86569,
      "PUERTO GUZMÁN" = 86571,
      "LEGUÍZAMO" = 86573,
      "SIBUNDOY" = 86749,
      "SAN FRANCISCO" = 86755,
      "SAN MIGUEL" = 86757,
      "SANTIAGO" = 86760,
      "VALLE DEL GUAMUEZ" = 86865,
      "VILLAGARZÓN" = 86885,
      "SAN ANDRÉS" = 88001,
      "PROVIDENCIA" = 88564,
      "LETICIA" = 91001,
      "EL ENCANTO" = 91263,
      "LA CHORRERA" = 91405,
      "LA PEDRERA" = 91407,
      "LA VICTORIA" = 91430,
      "MIRITI - PARANÁ" = 91460,
      "PUERTO ALEGRÍA" = 91530,
      "PUERTO ARICA" = 91536,
      "PUERTO NARIÑO" = 91540,
      "PUERTO SANTANDER" = 91669,
      "TARAPACÁ" = 91798,
      "INÍRIDA" = 94001,
      "BARRANCO MINAS" = 94343,
      "MAPIRIPANA" = 94663,
      "SAN FELIPE" = 94883,
      "PUERTO COLOMBIA" = 94884,
      "LA GUADALUPE" = 94885,
      "CACAHUAL" = 94886,
      "PANA PANA" = 94887,
      "MORICHAL" = 94888,
      "SAN JOSÉ DEL GUAVIARE" = 95001,
      "CALAMAR" = 95015,
      "EL RETORNO" = 95025,
      "MIRAFLORES" = 95200,
      "MITÚ" = 97001,
      "CARURU" = 97161,
      "PACOA" = 97511,
      "TARAIRA" = 97666,
      "PAPUNAUA" = 97777,
      "YAVARATÉ" = 97889,
      "PUERTO CARREÑO" = 99001,
      "LA PRIMAVERA" = 99524,
      "SANTA ROSALÍA" = 99624,
      "CUMARIBO" = 99773
      
    )
    allPlaces[unlist(allPlaces) %in% placeCodes]
  })
  
  output$dropdown <- renderUI({
    selectInput("place", "Seleccione el municipio:", choices = placeList(), selected = placeList()[1])
  })
  
  output$checkbox <- renderUI({
    req(epidata())
    checkboxInput("toggleSecondDropdown", "Comparar con otro municipio", FALSE)
  })
  
  output$second_dropdown <- renderUI({
    selectInput("second_place", "Seleccione el segundo municipio:", choices = placeList(), selected = placeList()[1])
  })
  
  # Dynamic UI for Slider
  output$yearSelector <- renderUI({
    req(epidata())
    fec_not <- as.Date(epidata()$fec_not)
    
    year_range <- lubridate::year(fec_not)
    year_range <- year_range[!is.na(year_range)] # remove NA values
    unique_years <- unique(year_range) # get unique years
    
    selectInput(inputId = "year",
                label = "Año:",
                choices = unique_years,
                selected = min(unique_years))
  })
  
  
  data_for_place <- reactive({
    req(epidata())
    selectedPlace <- input$place
    if (is.null(selectedPlace)) {
      selectedPlace <- 73001
    }
    epidata()[epidata()$cod_mun_o == selectedPlace, ]
    data_place_file <<- epidata()[epidata()$cod_mun_o == selectedPlace, ]
  })
  
  second_data_for_place <- reactive({
    req(epidata())
    selectedPlace <- input$second_place
    if (is.null(selectedPlace)) {
      selectedPlace <- 73001
    }
    epidata()[epidata()$cod_mun_o == selectedPlace, ]
    data_place_file <<- epidata()[epidata()$cod_mun_o == selectedPlace, ]
  })
  
  data_for_year <- reactive({
    req(data_for_place())
    data_for_place()[lubridate::year(data_for_place()$fec_not) == input$year, ]
    data_year_file <<- data_for_place()[lubridate::year(data_for_place()$fec_not) == input$year, ]
  })
  
  second_data_for_year <- reactive({
    req(second_data_for_place())
    second_data_for_place()[lubridate::year(second_data_for_place()$fec_not) == input$year, ]
    data_year_file <<- second_data_for_place()[lubridate::year(second_data_for_place()$fec_not) == input$year, ]
  })
  
  # Grafico de poblacion piramidal
  output$populationPyramid <- renderPlot({
    req(epidata())
    selectedPlace <- input$place
    if(is.null(selectedPlace) ){
      selectedPlace <- 73001
    }
    
    pyramid <<- population_pyramid(divipola_code = selectedPlace,
                                   year = input$year,
                                   range = 5,
                                   language = "ES",
                                   plot = TRUE,
                                   total = TRUE)
  })
  
  output$second_populationPyramid <- renderPlot({
    req(epidata())
    selectedPlace <- input$second_place
    if(is.null(selectedPlace) ){
      selectedPlace <- 73001
    }
    
    
    pyramid <<- population_pyramid(divipola_code = selectedPlace,
                                   year = input$year,
                                   range = 5,
                                   language = "ES",
                                   plot = TRUE,
                                   total = TRUE)
  })
  
  
  # Grafico tasa de incidencia
  output$incidenceRate <- renderPlot({
    req(data_for_year())
    selectedPlace <- input$place
    
    incidence_rate <- age_risk(
      age = as.integer(data_for_year()$edad),
      population_pyramid = pyramid,
      language = "ES",
      plot = TRUE
    )
  })
  
  output$second_incidenceRate <- renderPlot({
    req(second_data_for_year())
    selectedPlace <- input$second_place
    
    incidence_rate <- age_risk(
      age = as.integer(second_data_for_year()$edad),
      population_pyramid = pyramid,
      language = "ES",
      plot = TRUE
    )
  })
  
  # Grafico ocupaciones
  output$occupationPlot <- renderPlot({
    req(data_for_year())
    selectedPlace <- input$place
    year <- input$year
    
    data("isco88_table")
    describe_occupation(
      isco_codes = as.integer(data_for_year()$ocupacion),
      plot = "treemap"
    )
    plotLoaded(TRUE)
  })
  
  output$second_occupationPlot <- renderPlot({
    req(second_data_for_year())
    selectedPlace <- input$second_place
    year <- input$year
    
    data("isco88_table")
    describe_occupation(
      isco_codes = as.integer(second_data_for_year()$ocupacion),
      plot = "treemap"
    )
  })
  
  # Grafico Canal endemico
  output$endemicChannel <- renderPlot({
    req(data_for_year())
    selectedPlace <- input$place
    
    incidence_ibague <- incidence(
      dates = data_for_place()$fec_not,
      interval = "1 week"
    )
    
    # Se toma el historico de casos previo al 2018 para construir el canal endémico
    incidence_historic <- incidence_ibague[
      incidence_ibague$date <= as.Date("2018-12-31"), ]
    
    # Se toman el conteo de casos del 2019 como las observaciones
    observations <- incidence_ibague[
      incidence_ibague$date >= as.Date("2019-01-01") &
        incidence_ibague$date <= as.Date("2019-12-31"), ]$counts[,1]
    
    # Se especifican los años hiper endemicos que deben ser ignorados en la
    # constucción del canal endémico
    outlier_years <- 2016
    
    # Se construye el canal endémico y se plotea el resultado.
    tolima_endemic_chanel <- endemic_channel(
      incidence_historic = incidence_historic,
      observations = observations,
      outlier_years = outlier_years,
      language = "ES",
      plot = TRUE
    )
  })
  
  output$second_endemicChannel <- renderPlot({
    req(second_data_for_year())
    selectedPlace <- input$second_place
    
    incidence_ibague <- incidence(
      dates = second_data_for_place()$fec_not,
      interval = "1 week"
    )
    
    # Se toma el historico de casos previo al 2018 para construir el canal endémico
    incidence_historic <- incidence_ibague[
      incidence_ibague$date <= as.Date("2018-12-31"), ]
    
    # Se toman el conteo de casos del 2019 como las observaciones
    observations <- incidence_ibague[
      incidence_ibague$date >= as.Date("2019-01-01") &
        incidence_ibague$date <= as.Date("2019-12-31"), ]$counts[,1]
    
    # Se especifican los años hiper endemicos que deben ser ignorados en la
    # constucción del canal endémico
    outlier_years <- 2016
    
    # Se construye el canal endémico y se plotea el resultado.
    tolima_endemic_chanel <- endemic_channel(
      incidence_historic = incidence_historic,
      observations = observations,
      outlier_years = outlier_years,
      language = "ES",
      plot = TRUE
    )
  })
  
  # Mapa de indices Moran
  output$moranIndex <- renderLeaflet({
    req(data_for_year())
    data_tolima <- epidata_file[lubridate::year(epidata_file$fec_not) == input$year, ]
    incidence_object <- incidence(
      dates = data_tolima$fec_not,
      groups = data_tolima$cod_mun_o,
      interval = "12 months"
    )
    
    monrans_tolima <- morans_index(incidence_object = incidence_object, language = "ES")
    monrans_tolima$plot
  })
  
  output$selected_var <- renderText({
    base_text <- paste("Pirámide poblacional para", 
                       names(which(unlist(placeList()) == input$place)),
                       "en", 
                       input$year)
    
    if(input$toggleSecondDropdown) {
      additional_text <- names(which(unlist(placeList()) == input$second_place))
      full_text <- paste(base_text, "comparado con", additional_text)
    } else {
      full_text <- base_text
    }
    
    full_text
  })
  
  output$plotLoaded <- reactive({
    plotLoaded()
  })
  
  outputOptions(output, "plotLoaded", suspendWhenHidden = FALSE)
  
}

# Run the application
app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 8180, launch.browser = FALSE)