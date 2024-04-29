##### R Script for cleaning & combining nest hab data ####
################# November 18, 2021 ######################

#### packages needed ####
library(here)
library(dplyr)
library(tidyverse)

#### load data ####
nesthab1 <- read.csv(here::here("datatemp/original/NestHab2021/NestHabSurvey123_Final_7.30.21.csv"))
nesthab2 <- read.csv(here::here("datatemp/original/NestHab2021/NestHabToEnter.csv"))

#### join datasets ####
nesthab2021 <- full_join(nesthab2, nesthab1)

#### selected needed column labels & renamae to match 2020 nest habs ####
nesthab2021 <- nesthab2021 %>% dplyr::select(Nest.Name, Site, Location, 
                                        Nest.Hab.ID..NestName.Nest..NestName.Random1..or.NestName.Random2., 
                                        Distance.from.nest..m., Bearing.from.nest, 
                                        Date.of.Nest.Hab, Days.since.fledge.predicted.fledge, 
                                        Observer.initials, Number.of.dots.visible.from.1.5m.directly.above, 
                                        Number.of.dots.visible.from.1m.East, Number.of.dots.visible.from.1m.North,
                                        Number.of.dots.visible.from.1m.South, Number.of.dots.visible.from.1m.West, 
                                        Number.of.dots.seen.from.1m.North, Effective.Height...East, 
                                        Effective.Height...North, Effective.Height...South, Effective.Height...West, 
                                        Grass.Rank, Forb.Rank, Shrub.Rank, Other.Rank, 
                                        Most.dominant.grass, X2nd.most.dominant.grass, Most.dominant.forb, 
                                        X2nd.most.dominant.forb, Most.dominant.shrub, X2nd.most.dominant.shrub,
                                        Most.dominant.other, X2nd.most.dominant.other, Is.soil.crust.present., 
                                        if.the.flood.was.irrigated.or.there.was.high.water.from.river.flooding.or.rainfall..would.the.nest.likely.flood.,
                                        Compass.bearing.of.hill.face, Distance.in.meters.to.nearest.suitable.cover.for.concealing.chicks,
                                        Cover.type, Number.of.cowpies.softball.size.or.larger.within.a.3m.radius,
                                        Distance.in.cm.to.nearest.cowpie, Number.of.conspicuous.objects.in.3m.radius, 
                                        Distance.in.cm.to.nearest.conspicuous.object, What.is.the.conspicous.object., 
                                        Distance.in.meters.to.closest.road, Road.1.surface.type, Road.1.number.of.lanes,
                                        Distance.in.meters.to.next.closest.road, Road.2...surface.type, Road.2.number.of.lanes,
                                        Distance.in.m.to.nearest.perch, Height.in.m.of.nearest.perch, Type.of.perch, 
                                        Distance.in.m.to.nearest.perch.1, Height.in.m.of.nearest.perch.1, Type.of.perch.1, 
                                        Number.of.fresh.badger.mounds.in.10m.radius, Distance.in.meters.to.closest.badger.mound,
                                        Number.of.fresh.ground.squirrel.holes.in.10m.radius, Distance.in.meters.to.closest.ground.squirrel.hole,
                                        Nest.only..depth.of.nest.cup..cm., Nest.only..width..E.W..of.nest.cup..cm.,
                                        Nest.only..length..N.S..of.nest.cup..cm., Nest.only..Is.the.nest.oriented.toward.any.vegetation.,
                                        Nest.only..Bearing.from.the.nest.to.the.vegetation.it.is.oriented.toward, Nest.only..Notes.about.nest.orientation,
                                        Nest.only..Is.the.nest.decorated., Nest.only..length..N.S..of.nest.decoration..cm., Nest.only..width..E.W..of.nest.decoration..cm.,
                                        Nest.only..number.of.discrete.pieces.of.decoration, Nest.only..describe.decoration.type, Record.any.notes.about.the.nest.cup, 
                                        Notes)

nesthab2021 <- nesthab2021 %>% rename(
  ID = Nest.Name, 
  HabID = Nest.Hab.ID..NestName.Nest..NestName.Random1..or.NestName.Random2.,
  NestHabDate = Date.of.Nest.Hab, 
  DaysPostFledge = Days.since.fledge.predicted.fledge, 
  NestHabObs = Observer.initials, 
  Toward_A = Number.of.dots.visible.from.1.5m.directly.above, 
  Toward_N = Number.of.dots.visible.from.1m.North, 
  Toward_E = Number.of.dots.visible.from.1m.East, 
  Toward_W = Number.of.dots.visible.from.1m.West, 
  Toward_S = Number.of.dots.visible.from.1m.South,
  Toward_Type = Number.of.dots.seen.from.1m.North, 
  From_N = Effective.Height...North, 
  From_E = Effective.Height...East, 
  From_S = Effective.Height...South, 
  From_W = Effective.Height...West, 
  Grass_rank = Grass.Rank, 
  Forb_rank = Forb.Rank, 
  Shrub_rank = Shrub.Rank, 
  Other_rank = Other.Rank, 
  Grass1 = Most.dominant.grass, 
  Grass2 = X2nd.most.dominant.grass, 
  Forb1 = Most.dominant.forb, 
  Forb2 = X2nd.most.dominant.forb, 
  Shrub1 = Most.dominant.shrub, 
  Shrub2 = X2nd.most.dominant.shrub, 
  Other1 = Most.dominant.other, 
  Other2 = X2nd.most.dominant.other, 
  SoilCrust = Is.soil.crust.present., 
  FloodRisk = if.the.flood.was.irrigated.or.there.was.high.water.from.river.flooding.or.rainfall..would.the.nest.likely.flood., 
  Aspect = Compass.bearing.of.hill.face, 
  NearestCover = Distance.in.meters.to.nearest.suitable.cover.for.concealing.chicks, 
  CoverType = Cover.type, 
  CowP_Num = Number.of.cowpies.softball.size.or.larger.within.a.3m.radius, 
  CowP_Nearest = Distance.in.cm.to.nearest.cowpie, 
  Consp_Num = Number.of.conspicuous.objects.in.3m.radius, 
  Consp_Nearest = Distance.in.cm.to.nearest.conspicuous.object, 
  Consp_Type = What.is.the.conspicous.object., 
  Road1_Dist = Distance.in.meters.to.closest.road, 
  Road1_Surface = Road.1.surface.type, 
  Road1_Lanes = Road.1.number.of.lanes, 
  Road2_Dist = Distance.in.meters.to.next.closest.road, 
  Road2_Surface = Road.2...surface.type, 
  Road2_Lanes = Road.2.number.of.lanes, 
  Perch1_dist = Distance.in.m.to.nearest.perch, 
  Perch1_height = Height.in.m.of.nearest.perch, 
  Perch1_type = Type.of.perch, 
  Perch2_dist = Distance.in.m.to.nearest.perch.1, 
  Perch2_height = Height.in.m.of.nearest.perch.1, 
  Perch2_type = Type.of.perch.1, 
  Badger_num = Number.of.fresh.badger.mounds.in.10m.radius, 
  Badger_dist = Distance.in.meters.to.closest.badger.mound,
  GRSQ_num = Number.of.fresh.ground.squirrel.holes.in.10m.radius,
  GRSQ_dist = Distance.in.meters.to.closest.ground.squirrel.hole, 
  Nest_depth = Nest.only..depth.of.nest.cup..cm., 
  Nest_width = Nest.only..width..E.W..of.nest.cup..cm.,
  Nest_length = Nest.only..length..N.S..of.nest.cup..cm., 
  Nest_orient_to = Nest.only..Is.the.nest.oriented.toward.any.vegetation., 
  Nest_orient_degrees = Nest.only..Bearing.from.the.nest.to.the.vegetation.it.is.oriented.toward, 
  Nest_orient_notes = Nest.only..Notes.about.nest.orientation, 
  Nest_decorate = Nest.only..Is.the.nest.decorated., 
  Decorate_length = Nest.only..length..N.S..of.nest.decoration..cm., 
  Decorate_width = Nest.only..width..E.W..of.nest.decoration..cm., 
  Decoration_num = Nest.only..number.of.discrete.pieces.of.decoration,
  Decoration_type = Nest.only..describe.decoration.type, 
  NestCupNotes = Record.any.notes.about.the.nest.cup
)


nesthab2021 <- nesthab2021 %>%
  mutate(
    Year = 2021, 
    Veg.yn = 1, 
    Location = case_when(
      Location == "CC" ~ "CinderCone",
      Location == "PV" ~ "PleasantValley"
    )
  )


nesthab2021$Distance.from.nest..m. <- nesthab2021$Distance.from.nest..m. %>% replace(is.na(.), 0)

#Fix nest names
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV1-DeadLBCU" = "PV-DeadLBCU")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "Cc2-nesthole" = "CC-NestHole")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Brushline " = "CC-Brushline")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Decorator " = "CC-Decorator")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Alone " = "CC-Alone")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV-notaraptor" = "PV-NotARaptor")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-PleasentValley" = "CC-PleasantValley")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Circle " = "CC-Circle")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Paparazzi " = "CC-Paparazzi")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Sanded " = "CC-Sanded")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV1-Lookout" = "PV-Lookout")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "Cc2-waitwait" = "CC-WaitWait")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-RockCastle " = "CC-RockCastle")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV-strawberrymoon" = "PV-StrawberryMoon")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV-Technicolor " = "PV-Technicolor")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Waffle " = "CC-Waffle")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Double ClumpCC" = "CC-DoubleClump")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC2-NestHole" = "CC-NestHole")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV-Granola " = "PV-Granola")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "Cc1-cowpiecanopy" = "CC-CowpieCanopy")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Stealth " = "CC-Stealth")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV-Rabbit Fluff" = "PV-RabbitFluff")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "PV-grasshopper" = "PV-Grasshopper")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC1-Olive" = "CC-Olive")
nesthab2021$ID <- dplyr::recode(nesthab2021$ID, "CC-Rattler " = "CC-Rattler")














