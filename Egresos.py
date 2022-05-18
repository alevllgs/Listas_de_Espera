import pandas as pd
file = pd.read_excel('C:\Users\control.gestion3\OneDrive\BBDD Produccion\Listas de Espera\Listas de Espera DATA DEIS\BBDD LE DataDEIS\EGRESOS_2021.xlsx')
egresos = pd.DataFrame(file)
egresos
