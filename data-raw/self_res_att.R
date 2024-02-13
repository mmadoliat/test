vars <- c(
  "id" = "participant unique ID",
  "sex" = "sex of the participant (male/female)",
  "ethgroup" = "ethnic group of the participant (east_asian/west_asian/white)",
  "age" = "age of the participant in years",
  "m_non" = "mean number of times the other group members chose that male face as more attractive",
  "f_non" = "mean number of times the other group members chose that female face as more attractive",
  "m_self" = "number of times out of a possible 6 chose their male self-res face as more attractive",
  "f_self" = "number of times out of a possible 6 chose their female self-res face as more attractive",
  "grpsize" = "size of the group",
  "group" = "unique group ID",
  "mascpref" = "masculinity preference on an unrelated face preference task",
  "obro" = "number of older brothers",
  "osis" = "number of older sisters",
  "ybro" = "number of younger brothers",
  "ysis" = "number of younger sisters",
  "birthorder" = "birth order (only/firstborn/middleborn/lastborn) as calculated from number of younger and older brothers and sisters"
)
glue::glue("#'   \\item{[colname]}{[coldesc]}",
           colname = names(vars),
           coldesc = vars,
           .open = "[",
           .close = "]")

