import os, subprocess, sys, zipfile, smtplib
import os.path
import re
import shutil
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText


def linecounter(filename):
    ext = filename.split(".")[-1]
    file = open(filename, "r").read().replace(" ", "").replace("\t", "").splitlines()
    lines = 0
    for line in file:
        if len(line) > 0:
            if ext == "c" or ext == "scala":
                if line[:2] != "/*" and line[0] != "*" and line[:2] != "*/" and line[:2] != "//":
                    lines += 1
            elif ext == "clj" and line[0] != ";":
                lines += 1
            elif ext == "py" and line[0] != "#":
                lines += 1
            elif ext == "pl":
                if line[:2] != "/*" and line[0] != "*" and line[:2] != "*/" and line[0] != "%":
                    lines += 1

    return lines


def getwords(filename):
    id = set()
    with open(filename, "r") as opener:

        for currLine in opener:
            currLine = re.sub("(\#)+.*(?=\s)", "", currLine)
            currLine = re.sub("\"\s*.*?\s*\"", "", currLine)
            currLine = re.sub("(\%)+.*(?=\s)", "", currLine)
            currLine = re.sub("(\//.*)", "", currLine)
            currLine = re.sub("(\;.*)", "", currLine)
            currLine = re.findall("(\s*[a-zA-Z]*\s*)", currLine)
            for y in currLine:
                y = y.rstrip('\t\r\n')
                y = re.sub("\s+", "", y)
                y = y.rstrip('\n')
                if id.__contains__(y) is False:
                    id.add(y)

    return sorted(id)


def writeHTML(files, linecount, count, id):
    # Create line count
    html = open("summary_a" + str(count) + ".html", 'w')
    html.write("<html><body bgcolor='F0F8FF'><center><br>")
    html.write("<font size = 6>")
    html.write("Number of lines: " + str(linecount) + "<br>")
    html.write("</body>")

    # Create source file link
    href = "../" + "Project5\\project" + str(count)
    html.write("<font size=6")
    html.write("<br><a href='" + href + "'>" + files + "</a>")
    html.write("<br><br>")

    # Create identifiers list
    html.write("List of identifiers:" + "<br>")
    for identifier in id:
        html.write("<ul>" + identifier)
        html.write("</ul>")

    html.write("</html>")
    # Create index.html page
    html = open('index.html', '+w')
    html.write("<html><body bgcolor='F0F8FF'><center><br>")
    html.write("<font size='12'>")
    html.write("<b>CSC344 : Programming Languages</b><br>")
    html.write("Quan Trinh<br>")
    c = 1
    while c < 6:
        summaries = 'summary_a' + str(c) + ".html"
        html.write("<br><a href=" + summaries + '>Assignment ' + str(c) + '</a>')
        c += 1

    html.write("<br></center></body></html>")
    html.close()


def myzipper(system):
    myzipfile = zipfile.ZipFile("Project5.zip", 'w', zipfile.ZIP_DEFLATED)
    for curr in os.listdir(system):
        if os.path.isdir((system + curr + "/")):
            for filenext in os.listdir(system + "/" + curr):
                myzipfile.write(system + "/" + curr + "/" + filenext, "/Project5/" + curr + "/" + filenext)
        else:
            myzipfile.write(system + "/" + curr, "/Summaries_and_Index/" + curr)
    myzipfile.close()


def emailzipfile(givenemail):
    userName = "qtrinh@oswego.edu"
    password = "tnqNP000"
    myZipFile = "Project5.zip"
    attach = open(myZipFile, "rb")

    msg = MIMEMultipart()
    msg['From'] = userName
    msg['To'] = givenemail

    part = MIMEApplication(attach.read(), Name="Project5.zip")
    msg.attach(part)
    part.add_header('Content-Disposition', "attachment; filename= %s" % myZipFile)

    server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
    server.login(userName, password)
    server.sendmail(userName, givenemail, msg.as_string())
    server.close()


def main():
    my_path = os.path.dirname(__file__)
    # print("my path: " + my_path)
    path = os.path.join(my_path, "C:/Users/trinh/OneDrive/Documents/CSC344/")
    files = ["a1.c", "a2.clj", "a3.scala", "a4.pl", "a5.py"]
    q = 0
    z = 1
    while q < 5:
        # print("FILES AT Q: " + files[q])
        writeHTML(files[q], linecounter(path + "project" + str(q + 1) + "/" + files[q]), z,
                  getwords(path + "project" + str(q + 1) + "/" + files[q]))
        q += 1
        z += 1

    p = 1

    while p <= 5:
        shutil.move(my_path + "/summary_a%s.html" % p, "C:/Users/trinh/OneDrive/Documents/CSC344/")
        p += 1
    shutil.move(my_path + "/index.html", "C:/Users/trinh/OneDrive/Documents/CSC344/")
    myzipper("C:/Users/trinh/OneDrive/Documents/CSC344/")
    emailme = input("Email: ")
    emailzipfile(emailme)


if __name__ == "__main__":
    main()
