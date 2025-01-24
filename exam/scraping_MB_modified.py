# Packages
import requests  # The requests library is the de facto standard for making HTTP requests and has to be installed
import multiprocessing
import pandas as pd

# Saving HTML files -

# this part is taken almost identically from Matthias's code in the class. I have commented each
# step with what I understood the purpose of the code to be. I have only kept the loop that was actually run (that is,
# I have removed the part where it was constructed). While I have included the multiprocessing code, I did not use it
# this time. I had used the multiprocessing code to speed up the first time we did this for class, and it works for me.
# I used two processes at a time with intervals of 5000-10000 as that was what worked fastest on my system.
# Additionally, I have changed the Unhappy/Happy output to a dataframe which saves the
# location id and status code for each page in a log. However, if using multiple processes at once, the log file created
# will only display the results of the second (or last process). I couldn't find a way to index the processes to include
# multiple logs. Therefore, I have also included the code for when we run a single process and I manually updated
# the log file to create a single file which I have included in the results. If you don't want an external file,
# replace the with-open commmand with return and the results are shown in the console. I ran the loop for the range
# 3 - 50000 and 235000-250000. I had 20193 HTML files at the end.

# Multiprocessing

def get_pages(begin, end):  # creating a function called get_pages
    l1 = []  # create two empty lists l1 and l2 (this part of the code to replace the Happy/Unhappy code)
    l2 = []
    for loc_id in range(begin, end):
        url = 'https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey=' + str(
            loc_id)  # the webpage to be scraped saved in variable url and str() converts the numeric location id to
        # a string
        page = requests.get(url)  # page is name of variable which saves the results from reading the url
        l1.append(loc_id)    # append the location id to l1
        v2 = page.status_code
        l2.append(v2)   # and append the status code to l2
        print(loc_id)  # kept this to keep track of where we are in loop
        if page.status_code == 200:  # .status_code == 200 implies good and 100,300-500 have problems
            with open('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/file' + str(
                    loc_id) + '.html',
                      'wb+') as f:  # The with statement creates a context manager that simplify the way files are
                    # opened and closed in Python programs. It automatically closes each file it opens after use saving
                    # on memory
                f.write(
                    page.content)  # write.csv in R - f is the object and then we want to write it out as HTML -
                # the content of the variable page
    d = pd.DataFrame()  # Create a dataframe which saves the output of the lists l1 and l2
    d['l1'] = l1
    d['l2'] = l2
    with open('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/log' + '.txt', 'w') as f:
        df = d.to_string(header=False, index=False)  # save this dataframe (with each iteration) to the file 'log.txt'
        f.write(df)


if __name__ == "__main__":  # __name__ == "__main__" condition is used here to run this condition only if the code
    # is run directly by the interpreter and does not run if imported as part of a module. this is used to include code
    # that is not run if another program runs this module.
    p1 = multiprocessing.Process(target=get_pages,
                                 args=(20, 25))  # Define two processes which use the get_pages function for arguments
    # specified. Can increase both the number of processes defined as well as range of arguments, but there is a
    # memory tradeoff.
    p2 = multiprocessing.Process(target=get_pages, args=(50005, 50015))

    p1.start()  # start the process
    p2.start()

    p1.join()  # wait for the process to end with join()
    p2.join()

# Without multiprocessing
l1 = []  # create two empty lists l1 and l2 (this part of the code to replace the Happy/Unhappy code)
l2 = []

for loc_id in range(235000,250000):  # the range is specified for each iteration and will show the last range I used
    url = 'https://theclergydatabase.org.uk/jsp/locations/' \
          'DisplayLocation.jsp?locKey='+str(loc_id) # the webpage to be scraped saved in variable url and str()
    # converts the numeric location id to a string
    # page is name of variable which saves the results from reading the url
    l1.append(loc_id)  # append the location id to l1
    v2 = page.status_code
    l2.append(v2)  # and append the status code to l2
    print(loc_id)  # kept this to keep track of where we are in loop
    if page.status_code == 200:
        with open('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/file'+str(loc_id)+'.html','wb+') as f:
            f.write(page.content)

d = pd.DataFrame()  # Create a dataframe which saves the output of the lists l1 and l2
d['l1'] = l1
d['l2'] = l2
with open('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/log' + '.txt', 'w') as f:
    df = d.to_string(header=False, index=False)  # save this dataframe (with each iteration) to the file 'log.txt'
    f.write(df)

# Processing files
# The code is almost identical to what we had in class, comments indicate  what I understand the code to be doing.
# This code reads all the HTML files and then processes them to create two types of files, location files which include
# information on where the church entity is located and data files which contain information about the entities.
# The number on each of these files (and the original HTML files) indicates the cced_id, a unique identifier for each
# location of a church/similar entity.

# Packages
from os import listdir
from os.path import isfile, join
import pandas as pd
from bs4 import BeautifulSoup
# Beautiful Soup is a Python library for pulling data out of HTML and XML files. It works with the defined parser to
# provide ways of navigating, searching, and modifying the parsed content.

# Startup code
# os.listdir () method is used to get the list of all files and directories in the specified directory, here the path
# to where we saved the HTML files.
# join(,) concatenates the strings given as arguments
# isfile() method returns a Boolean value of class bool. This method returns True if specified path is an existing
# regular file, otherwise returns False.
# This code creates a list 'files' which contains the names of all the files in the folder with saved files, if these
# files are regular files and work.
files = [f for f in listdir('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/') if isfile(join('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/',f))]
print(len(files))

# Processing with multiple files:
for f in files:   # for each element in the list 'files'
    print(f)   # print the name of the element
    with open('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Input/'+str(f),'rb') as file:
        # open the element and read it in binary mode, assign it the label file
        soup = BeautifulSoup(file.read(),'html.parser')  # create a Beautiful Soup object (called 'soup') that takes
        # the reading of the element, which is the HTML content scraped earlier, as its input. 'html.parser' ensures
        # the use of the right parser to read our HTML elements.

    #table = soup.find('div','tb s2')  # the HTML element 'div class = "tb s2"' contains the table 'Evidence' which is
    # one part of the information we want to extract. The variable table is assigned this information. Removed this part
    # when running the code
    # classes of each table:

    t1 = soup.find_all('table')  # find all occurrences of table in soup. There are two - the first one is the head of
    # table and the second one contains all the relevant information.
    if len(t1)>1:  # if the table with the relevant information exists, assign it to t2
        t2 = t1[1]  # python indexing starts with 0, so this assigns the second element of t1

        df = pd.DataFrame(columns=['Names','PersonID','Year','Type','Full'])  # create a dataframe with these colnames

        for row in t2.tbody.find_all('tr'):  # find all 'tr' in the body of the table 't2', where each 'tr' corresponds
            # to one row of the table and then find all 'td' which contain all the actual information as elements.
            #print(row)
            columns = row.find_all('td')  # store the information into the variable 'columns'
            if len(columns)>0:
                #print(columns)
                # the next steps clean the elements and save them to the variables as defined here.
                names = columns[0].text.strip().replace("\r","").replace("\n","").replace("  "," ")
                year = columns[1].text.strip().replace("\r", "").replace("\n", "").replace("  ", " ")
                type = columns[2].text.strip().replace("\r", "").replace("\n", "").replace("  ", " ")
                office = columns[3].text.strip().replace("\r", "").replace("\n", "").replace("  ", " ")
                full = columns[4].text.strip().replace("\r", "").replace("\n", "").replace("  ", " ")
                c = columns[0].find('a', href=True)   # this step reads the PersonID from the first element of columns
                # the variable c stores the first element of columns and defines href as an attribute
                if c.__str__()!='None':  # if c is not an empty string, define persid as the attribute href of c and
                    # with some replacement/cleaning
                    persid = c['href'].replace('../persons/CreatePersonFrames.jsp?PersonID=','')
                else:
                    persid = '0'

                # append to the empty dataframe df these values, matched to the column names
                df = df.append({'Names':names,'PersonID':persid,'Year':year,'Type':type, 'Office':office,'Full':full}, ignore_index=True)
        # Save file to Output folder by redefining name and file type. This is the data file
        f1 = f.replace('.html','').replace('file','')
        df.to_csv('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Output/data'+str(f1)+'.csv',index=False)

        # Extracting data from Summary and the header of the page about location
        l1 = soup.find('ul',{'class':'s2'})  # l1 contains all the information of the Summary table
        cols = []
        for row in l1.find_all('li'):  # find all 'li' in the Summary table - because each 'li' is 1 row of information
            # try - except lets python run code that might generate an error by providing another option (the except
            # code), if the try code produces an error. Here, we try to append the label directly to an empty
            # dataframe but if it doesn't work, we define it separately and then append it and also append 'parish'.
            try:
                cols.append(row.label.text.replace('\xa0',' ').replace("\r", "").replace("\n", "").replace("  ", " ").replace(":", ""))
                last = row.label.text.replace('\xa0',' ').replace("\r", "").replace("\n", "").replace("  ", " ")
            except:
                cols.append(last)
        cols.append('parish')
        # we then create a dataframe out of the cols variable, which is basically the Summary table from the HTML file
        df = pd.DataFrame(cols)
        # We then create another list (later transformed into a dataframe) and read the values of the cells into
        # the variable text, followed by appending it to the mentioned empty list.

        data = []
        for row in l1.find_all('li'):
            text = row.text.replace('\xa0',' ').replace("\r", "").replace("\n", "").replace("  ", " ")
            for c in range(0,len(cols)):
                text = text.replace(cols[c],"")
            data.append(text)

        l2 = soup.find('div',{'class':'ph'})  # This part stores the Location title into a dataframe.
        data.append(l2.text.replace('\n','').replace('\r',''))
        df2 = pd.DataFrame(data)
        frame = [df,df2]  # merge the two dataframes with information on location into one.
        final = pd.concat(frame,axis=1) # concatenate df and df2 horizontally
        final = final.transpose()  # transpose the file to get the right rows and columns
        final.to_csv('/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Output/Location'+str(f1)+'.csv',index=False)

# The cleaning and merging of the data and location files (total files : 36563) is in the r script 'church.r'.
