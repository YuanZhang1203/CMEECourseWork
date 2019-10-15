birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
)

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively.
latin_names = []
latin_names.append(birds[0][0])
latin_names.append(birds[1][0])
latin_names.append(birds[2][0])
latin_names.append(birds[3][0])
latin_names.append(birds[4][0])
print(latin_names) 

Common_names = []
Common_names.append(birds[0][1])
Common_names.append(birds[1][1])
Common_names.append(birds[2][1])
Common_names.append(birds[3][1])
Common_names.append(birds[4][1])
print(Common_names) 

mean_body_masses = [] 
mean_body_masses.append(birds[0][2])
mean_body_masses.append(birds[1][2])
mean_body_masses.append(birds[2][2])
mean_body_masses.append(birds[3][2])
mean_body_masses.append(birds[4][2])
print(mean_body_masses) 



# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !). 
latin_names = [] 
for i in range(5):
	latin_names.append(birds[i][0])
print(latin_names)

Common_names = [] 
for i in range(5):
	Common_names.append(birds[i][1])
print(Common_names)

mean_body_masses = [] 
for i in range(5):
	mean_body_masses.append(birds[i][2])
print(mean_body_masses)
