import random
with open('estados.txt','a') as f:
    for j in range(100):
       lista = ['morto','vivo','zumbi'] 
       i = random.choice(lista)
       f.write(i+"\n")
f.close()
