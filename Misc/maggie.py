print("Hello Filip!")

months = ["      ","jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "okt", "nov", "dec"]
persons = ["      ", "Maggie", "Filip ", "Oliver", "Emelie", "Hannah"]

w, h = 13, 7;
Matrix = [["   " for x in range(w)] for y in range(h)]
Matrix[0] = months
print(Matrix)

Matrix[0:5][0] = persons

for eye in range(h-1):

    Matrix[eye][0] = persons[eye]
    for i in range(w-1):

        print(str("| "+str(Matrix[eye][i])+" "),end='')
    print("|")
