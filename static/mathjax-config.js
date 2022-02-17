MathJax = {
    tex: {
        inlineMath: [['$', '$']],
        displayMath: [['$$', '$$']]
    },
    options: {
        skipHtmlTags: [
            'script', 'noscript', 'style', 'textarea', 'pre',
            'code', 'annotation', 'annotation-xml',
            'decl', 'decl_meta', 'attributes', 'decl_args', 'decl_header', 'decl_name',
            'decl_type', 'equation', 'equations', 'structure_field', 'structure_fields',
            'constructor', 'constructors', 'instances'
        ],
        ignoreHtmlClass: 'tex2jax_ignore',
        processHtmlClass: 'tex2jax_process',
    },
};