import re


def remove_matches(regex, file_contents):
    match = re.search(regex, file_contents, re.MULTILINE)
    while match is not None:
        file_contents = file_contents[:match.start()] + file_contents[match.end():]
        match = re.search(regex, file_contents, re.MULTILINE)
    return file_contents


def parse_file(filename):
    file_contents = open(filename).read()
    comment_regex = r"(;; *.*)|(; *.*)"
    file_contents = remove_matches(comment_regex, file_contents)

    string_regex = r"(\'[^\']+\')|(\"[^\"]+\")"
    file_contents = remove_matches(string_regex, file_contents)

    identifier_regex = r"[a-zA-Z]+[a-zA-Z_0-9?-]*"
    matches = re.finditer(identifier_regex, file_contents)
    identifiers = set()
    for match in matches:
        identifiers.add(file_contents[match.start():match.end()])

    return identifiers

