# Codigo de correccion de especies

# 
baseDatos <- readRDS(file = "../00baseDatos/base_datos.RDS")
especies <- baseDatos$especies
especies$especie

especie$especie[especie$especie=="chalicodoma albocristata"] <- "Megachile albocristata"
especie$especie[especie$especie=="chalicodoma ericetorum"] <- "Megachile ericetorum"
especie$especie[especie$especie=="chalicodoma hungarica"] <- "Megachile hungarica"
especie$especie[especie$especie=="chalicodoma lefebvrei"] <- "Megachile lefebvrei"
especie$especie[especie$especie=="chalicodoma manicata"] <- "Megachile manicata"
especie$especie[especie$especie=="chalicodoma montenegrensis"] <- "Megachile montenegrensis"
especie$especie[especie$especie=="chalicodoma parietina"] <- "Megachile parietina"
especie$especie[especie$especie=="chalicodoma pyrenaica"] <- "Megachile pyrenaica"
especie$especie[especie$especie=="chelostoma appendiculatum"] <- "Chelostoma emarginatum"
especie$especie[especie$especie=="coelioxis afra"] <- "Coelioxys mandibularis"
especie$especie[especie$especie=="coelioxys aurolimbata"] <- "Coelioxys aurolimbatus" # calza con sinonima de subespecie
# especie$especie[especie$especie=="coelioxys rufocaudata"] <- ""
especie$especie[especie$especie=="colletes spectabilis"] <- "Colletes albomaculatus"
especie$especie[especie$especie=="creightonella albisecta"] <- "Megachile albisecta"
especie$especie[especie$especie=="eoanthidium elongatum"] <- "Eoanthidium clypeare"
especie$especie[especie$especie=="eucera nigrescens contraria"] <- "Eucera longicornis"
especie$especie[especie$especie=="eucera nigrilabris rufitarsis"] <- "Eucera nigrilabris"
especie$especie[especie$especie=="halictus cephalicus"] <- "Seladonia cephalica"
especie$especie[especie$especie=="halictus gemmeus"] <- "Seladonia gemmea"
# especie$especie[especie$especie=="halictus pollinus"] <- ""
especie$especie[especie$especie=="halictus smaragdulus"] <- "Seladonia smaragdula"
especie$especie[especie$especie=="halictus subauratus"] <- "Seladonia subaurata"
# especie$especie[especie$especie=="halictus tectus"] <- ""
# especie$especie[especie$especie=="heriades coelostomus"] <- ""
# especie$especie[especie$especie=="heriades crenulatus"] <- ""
especie$especie[especie$especie=="heriades dalmaticus"] <- "Heriades punctulifera"
especie$especie[especie$especie=="heriades trucorum"] <- "Heriades truncorum"
especie$especie[especie$especie=="hoplitis crenulata"] <- "Hoplitis annulata" # revisar 
# especie$especie[especie$especie=="hoplitis rufohirta"] <- ""
# especie$especie[especie$especie=="hoplitis sybarita"] <- ""
especie$especie[especie$especie=="lasioglossum brevicome"] <- "Lasioglossum brevicorne"
especie$especie[especie$especie=="lasioglossum cristula donatum"] <- "Lasioglossum cristula" # revisar
especie$especie[especie$especie=="lasioglossum laticeps hellenicum"] <- "Lasioglossum laticeps" # revisar
especie$especie[especie$especie=="lasioglossum leucozonium clusium"] <- "Lasioglossum leucozonium" # revisar
especie$especie[especie$especie=="lasioglossum nitidulum fudakowskii"] <- "Lasioglossum nitidulum" # revisar
especie$especie[especie$especie=="lasioglossum obscuratum acerbum"] <- "Lasioglossum obscuratum"
especie$especie[especie$especie=="lasioglossum pygmaeum patulum"] <- "Lasioglossum pygmaeum"
especie$especie[especie$especie=="lasioglossum subaenescens asiaticum"] <- "Lasioglossum subaenescens"
especie$especie[especie$especie=="megachile giraudi bicoloriventris"] <- "Megachile giraudi"
# especie$especie[especie$especie=="megachile pacifica"] <- ""
especie$especie[especie$especie=="megachile pilidens"] <- "Megachile argentata"
# especie$especie[especie$especie=="megachile villipes"] <- ""
especie$especie[especie$especie=="melecta albifrons albovaria"] <- "Melecta albifrons"
especie$especie[especie$especie=="mesanthidium carduele"] <- "Afranthidium carduele" # revisar
especie$especie[especie$especie=="metallinella brevicornis"] <- "Osmia brevicornis"
especie$especie[especie$especie=="nomada callimorpha"] <- "Nomada calimorpha"
# especie$especie[especie$especie=="nomada nobilia"] <- "Nomada nobilis"
especie$especie[especie$especie=="nomada rubricolis"] <- "Nomada rubricollis"
especie$especie[especie$especie=="nomada succinta"] <- "Nomada succincta"
# especie$especie[especie$especie=="osmia fulviventris"] <- ""
especie$especie[especie$especie=="osmia latrellei"] <- "Osmia latreillei"
especie$especie[especie$especie=="osmia longiceps"] <- "Osmia cephalotes"
especie$especie[especie$especie=="osmia nicosiana"] <- "Osmia viridana"
# especie$especie[especie$especie=="proxylocopa olivieri"] <- ""
especie$especie[especie$especie=="pseudapis diversipes"] <- "Nomiapis diversipes"
# especie$especie[especie$especie=="pseudapis monstrosa"] <- ""
# especie$especie[especie$especie=="pseudapis unidentata"] <- ""
# especie$especie[especie$especie=="pseudoanthidium cribratum"] <- ""
especie$especie[especie$especie=="pseudoanthidium lituratum"] <- "Pseudoanthidium nanum"
# especie$especie[especie$especie=="pseudoheriades asiaticus"] <- ""
especie$especie[especie$especie=="pseudoheriades hofferi"] <- "Stenoheriades coelostoma"
especie$especie[especie$especie=="rophites algirus graecus"] <- "Rophites algirus" # revisar
especie$especie[especie$especie=="tetralonia alternans"] <- ""


# matriz 006
especie$especie[especie$especie=="andrena pubescens"] <- "Andrena nitida"
especie$especie[especie$especie=="bombus terrestris lucorum"] <- "Bombus terrestris" # por ahora omitimos subespecie
# especie$especie[especie$especie=="psithyrus vest"] <- ""

# matriz 007
# igual a 006

# matriz 10
# no hay especies de abejas pero podriamos usar promedio de genero bombus a nivel europeo


# matriz 17
# nada que agregar

# matriz 36
# nada que agregar

# matriz 38
# nada que agregar

# matriz 47
especie$especie[especie$especie=="halictus confusus"] <- "Seladonia confusa"

# matriz 50
especie$especie[especie$especie=="anthophora alluardii"] <- "Anthophora alluaudi"
especie$especie[especie$especie=="lasioglossum evylaeus"] <- ""


# Venezuela
# matriz 31
# especie$especie[especie$especie=="bombus atratus"] <- "bombus pauloensis"
especie$especie[especie$especie=="melipona compresipes"] <- "melipona interrupta"

# Argentina
# 

# estados unidos
# M_005
especie$especie[especie$especie=="andrena placida"] <- "andrena barbilabris"
especie$especie[especie$especie=="anthophora smithi"] <- "anthophora quadricolor"


especie$especie[especie$especie==""] <- ""



