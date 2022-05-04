
# Download arm 1 data
drive_download(file = as_id('https://drive.google.com/open?id=1n4zilEHIU-m3KaO3d6E-cULaqGdo4IcF'),
               path = '../Raw/captacion_tratamientos_1.xlsx', overwrite = T)


# Download arm 23 data
drive_download(file = as_id('https://drive.google.com/open?id=0ByXwiiVuTOVqUmN6SVJCQUNoNWs'),
               path = '../Raw/base_modulo_informacion.xlsm', overwrite = T)


# Download survey data: Excel File

drive_download(file = as_id('https://drive.google.com/open?id=1RphcVw47LiiLPL29gGxLb0MqpdgSC5Ig'),
               path = '../Raw/base_control_encuestas.xlsx', overwrite = T)


# Download survey data: Spreadsheet

drive_download(file = as_id('https://drive.google.com/open?id=1w1FBTXbsEYIeFVp5IKfVOTAo6pKi1RmXM_55EhShRl0'),
               path = '../Raw/gf2m', overwrite = T, type = "csv")

drive_download(file = as_id('https://drive.google.com/open?id=1-pX3OlFkg_j--JrqnMsw12ieqLc6CdZeUJnV0M9a2Sk'),
               path = '../Raw/gf2w', overwrite = T, type = "csv")


# Download follow-up end mode (for lawyer quality)

drive_download(file = as_id('https://drive.google.com/open?id=11I7HvZaEzAIFccvzCXqqcQFzua_Ac5LxfJB2-A_At_c'),
               path = '../Raw/seguimiento_dem.xlsx', overwrite = T)