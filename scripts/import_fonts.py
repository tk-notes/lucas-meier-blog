import os
import sys


WEIGHTS = {
    'ExtraLight': 100,
    'Light': 200,
    'SemiLight': 300,
    'Normal': 400,
    'Medium': 500,
    'Bold': 700,
    'ExtraBold': 800
}


def font_info(name):
    weight = WEIGHTS['Normal']
    for k, v in WEIGHTS.items():
        if k in name:
            weight = v
            break
    italic = 'Italic' in name
    family = name.split('-')[0]
    return (family, weight, italic)


def display_font_info(info, path):
    family, weight, italic = info
    items = []
    items.append('@font-face {')
    items.append(f'  font-family: "{family}";')
    items.append(f'  font-weight: {weight};')
    if italic:
        items.append('  font-style: italic;')
    extension = path.split('.')[-1]
    items.append(f'  src: url("{path}") format({extension});')
    items.append('}\n')
    return '\n'.join(items)


def main(font_name, dir, out):
    fonts = [p for p in os.listdir(dir) if p.startswith(font_name)]
    with open(out, 'a') as fp:
        for f in fonts:
            fp.write('\n')
            info = font_info(f)
            path = f'{dir}/{f}'
            fp.write(display_font_info(info, path))
        fp.write('\n')


if __name__ == '__main__':
    if len(sys.argv) < 4:
        print('Insufficient arguments')
    else:
        main(sys.argv[1], sys.argv[2], sys.argv[3])
  