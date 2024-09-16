import re
from glob import glob
from pathlib import Path
from bs4 import BeautifulSoup
from tqdm import tqdm

def minify_html(content):
    # Remove whitespace between HTML tags
    content = re.sub(r'>\s+<', '><', content)
    # Remove leading and trailing whitespace from lines
    content = re.sub(r'^\s+|\s+$', '', content, flags=re.MULTILINE)
    return content

def minify_equation(eq):
    # Preserve content but remove unnecessary whitespace
    eq_content = ' '.join(eq.stripped_strings)
    return f'<figure class="equation">{eq_content}</figure>'

htmlpaths = glob("**/*.html", recursive=True)
for htmlpath in tqdm(htmlpaths):
    htmlpath = Path(htmlpath)
    content = htmlpath.read_text(encoding="utf-8")
    soup = BeautifulSoup(content, "html.parser")

    equations = soup.find_all("figure", class_="equation")

    for eq in equations:
        eq.replace_with(BeautifulSoup(minify_equation(eq), "html.parser"))

    # Minify the entire HTML content
    minified_html = minify_html(str(soup))

    # if equations:
    #     # Print minified equations for verification
    #     soup = BeautifulSoup(minified_html, "html.parser")
    #     equations = soup.find_all("figure", class_="equation")
    #     print(f"Equations found: {len(equations)}")
    #     for eqn in equations:
    #         print(eqn)
    #         print()

    htmlpath.write_text(minified_html, encoding="utf-8")
