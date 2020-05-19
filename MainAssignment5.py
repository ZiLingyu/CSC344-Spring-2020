from bs4 import BeautifulSoup
import os
import Parse_C_Code
import Parse_Clojure_Code
import Parse_Scala_Code
import Parse_Prolog_Code
import Parse_Python_Code


def make_template(title):
    return '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">' \
           '<title>' + title + '</title></head><body></body></html>'


def add_tag(body, tag):
    body.append(BeautifulSoup(tag, "html.parser"))


def add_tags(tags, title):
    soup = BeautifulSoup(make_template(title), "html.parser")
    body = soup.select_one("body")
    for tag in tags:
        add_tag(body, tag)
    return soup


def num_line(file):
    returned_data = os.popen("wc -l " + file).read()
    return returned_data.split(" ")[0]


def get_identifiers(full_path):
    identifiers = []
    filename = full_path.split("/")[-1]
    if filename.split(".")[-1] == 'c':
        identifiers = Parse_C_Code.parse_file(full_path)
    elif filename.split(".")[-1] == "clj":
        identifiers = Parse_Clojure_Code.parse_file(full_path)
    elif filename.split(".")[-1] == "scala":
        identifiers = Parse_Scala_Code.parse_file(full_path)
    elif filename.split(".")[-1] == "pl":
        identifiers = Parse_Prolog_Code.parse_file(full_path)
    elif filename.split(".")[1] == "py":
        identifiers = Parse_Python_Code.parse_file(full_path)

    return identifiers


def process_project(directory):
    tags = []
    summary_output_directory = "HTML-Output"
    # If directory HTML-Output does not exist, make the directory
    if not os.path.isdir(summary_output_directory):
        os.mkdir(summary_output_directory)

    file = open(summary_output_directory + "/" + directory.split("/")[1] + "_summary.html", "w+")
    for filename in os.listdir(directory):
        if filename[0] != ".":
            full_path = directory + "/" + filename
            lines_in_file = num_line(full_path)
            file_tag = '<a href="' + "../" + full_path + '"><h1>' + filename + ' Identifiers, ' + lines_in_file + ' lines</h></a>'
            identifiers = get_identifiers(full_path)
            identifiers_tag = "<p>"
            for identifier in identifiers:
                identifiers_tag += identifier
                identifiers_tag += "<br>"
            identifiers_tag += "</p>"
            tags.append(file_tag)
            tags.append(identifiers_tag)
    html_file = add_tags(tags, directory.split("/")[1] + " Summary")
    file.write(str(html_file))

    file.close()


def create_index_file():
    file = open("HTML-Output/index.html", "w+")
    tags = ["<h1>Project Summaries</h1>"]
    for filename in os.listdir("HTML-Output"):
        if not filename == "index.html":
            tag = '<a href="' + filename + '"><h2>' + "/" + filename.split("_")[0] + '</h2></a>'
            tags.append(tag)

    html_file = add_tags(tags, "Project Summaries Index")
    file.write(str(html_file))
    file.close()
    os.system("tar -czf summary.tar.gz HTML-Output/ Projects/")


def send_email():
    send_email_address = input("Please enter email address: ")
    subject = input("Please enter subject: ")
    os.system('mutt -s "' + subject + '" -a summary.tar.gz < /dev/null -- ' + send_email_address)


def main():
    for directory in os.listdir("Projects"):
        if 'Project' in directory:
            process_project('Projects/' + directory)

    create_index_file()
    send_email()


main()
