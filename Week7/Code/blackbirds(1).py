import re

# Read the file (using a different, more python 3 way, just for fun!)
with open('../data/blackbirds.txt', 'r', encoding='utf-8') as f:
    text = f.read()
    for line in text.split('\n'):
        if 'Kingdom' in line:
            name = re.findall(r'Kingdom\t(.*)', line)[0]
            print(f'Kingdom: %s' % name.split('–')[-1].strip())
        elif 'Phylum' in line:
            name = re.findall(r'Phylum\t(.*)', line)[0]
            print(f'Phylum: %s' % name.split('–')[-1].strip())
        elif 'Species' in line:
            name = re.findall(r'Species\t(.*)', line)[0]
            print(f'Species: %s' % name.split('–')[-1].strip())

# replace \t's and \n's with a spaces:
text = text.replace('\t',' ')
text = text.replace('\n',' ')
# You may want to make other changes to the text. 

# In particular, note that there are "strange characters" (these are accents and
# non-ascii symbols) because we don't care for them, first transform to ASCII:

text = text.encode('ascii', 'ignore') # first encode into ascii bytes
text = text.decode('ascii', 'ignore') # Now decode back to string

# Now extend this script so that it captures the Kingdom, Phylum and Species
# name for each species and prints it out to screen neatly.

# Hint: you may want to use re.findall(my_reg, text)... Keep in mind that there
# are multiple ways to skin this cat! Your solution could involve multiple
# regular expression calls (easier!), or a single one (harder!)