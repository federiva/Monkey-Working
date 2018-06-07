# ParseHTMLTable

This script contains: 
* A function named parseHTML written in bash using awk and cat to parse a custom HTML table.
* An example on how to use the function.

## Getting Started

Usage:
In a terminal, using bash:

```
mv parseHTML parseHTML.sh
chmod +x parseHTML.sh
./parseHTML <file.html> 
```

It prints to stdout the first and fifth column in a row of an HTML table that follows the next structure: 

```
<tr>
<td class="lateral2">14/01/2002</td>
<td class="lateral2">S/D</td>
<td class="lateral2">S/D</td>
<td class="lateral2">1.40</td>
<td class="lateral2">1.60</td>
</tr>
```
In this case, an n-th iteration of the function outputs: 
'''
14/01/2002,1.60
'''

### Prerequisites

The script was written using: 

```
GNU Awk 4.0.1
GNU bash, version 4.3.11(1)-release (x86_64-pc-linux-gnu)
cat (GNU coreutils) 8.21
```

## Authors

* **Federico Rivadeneira** - *Initial work* - [federiva](https://github.com/federiva)

See also the list of [contributors](https://github.com/federiva/Monkeys-Working/graphs/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

* [Bruce Barnett](http://www.grymoire.com/Unix/Awk.html#TOC) and his guide to AWK.
* [Billie Thompson](https://gist.github.com/PurpleBooth) for providing a nice [README.md](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2) template used here.

