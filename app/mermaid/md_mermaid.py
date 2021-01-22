"""
Mermaid Extension for Python-Markdown
========================================

Adds mermaid parser (like github-markdown) to standard Python-Markdown code blocks.

Original code Copyright 2018-2020 [Olivier Ruelle].

License: GNU GPLv3

"""

from markdown.extensions import Extension
from markdown.preprocessors import Preprocessor
from PyQt5.QtCore import QUrl

import re
import os

MermaidRegex = re.compile(r"^(?P<mermaid_sign>[\~\`]){3}[\ \t]*[Mm]ermaid[\ \t]*$")


# ------------------ The Markdown Extension -------------------------------

class MermaidPreprocessor(Preprocessor):
    def run(self, lines):
        new_lines = []
        mermaid_sign = ""
        m_start = None
        m_end = None
        in_mermaid_code = False
        is_mermaid = False
        old_line = ""

        js_file = os.path.join(os.path.dirname(__file__), "node_modules", "mermaid", "dist", "mermaid.js")

        new_lines.append('<meta charset="utf-8">')
        new_lines.append('<script src="{}"></script>'.format(QUrl.fromLocalFile(js_file).toString()))
        new_lines.append('<script>mermaid.initialize({startOnLoad:true});</script>')

        for line in lines:
            # Wait for starting line with MermaidRegex (~~~ or ``` following by [mM]ermaid )
            if not in_mermaid_code:
                m_start = MermaidRegex.match(line)
            else:
                m_end = re.match(r"^["+mermaid_sign+"]{3}[\ \t]*$", line)
                if m_end:
                    in_mermaid_code = False

            if m_start:
                in_mermaid_code = True
                mermaid_sign = m_start.group("mermaid_sign")
                if not re.match(r"^[\ \t]*$", old_line):
                    new_lines.append("")
                if not is_mermaid:
                    is_mermaid = True
                new_lines.append('<div class="mermaid">')
                m_start = None
            elif m_end:
                new_lines.append('</div>')
                new_lines.append("")
                m_end = None
            elif in_mermaid_code:
                new_lines.append(line.strip())
            else:
                new_lines.append(line)

            old_line = line

        return new_lines


class MermaidExtension(Extension):
    """ Add source code hilighting to markdown codeblocks. """

    def extendMarkdown(self, md, md_globals):
        """ Add HilitePostprocessor to Markdown instance. """
        # Insert a preprocessor before ReferencePreprocessor
        md.preprocessors.register(MermaidPreprocessor(md), 'mermaid', 35)

        md.registerExtension(self)

def makeExtension(**kwargs):  # pragma: no cover
    return MermaidExtension(**kwargs)
