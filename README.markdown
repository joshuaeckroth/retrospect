# retrospect

## Requirements

- Java (JDK 5+)

- git (Windows: [msysgit](http://code.google.com/p/msysgit/)

- [Graphviz](http://www.graphviz.org/Download.php) (Available for Mac,
  Windows, and Linux)

## Installation (Unix)

- Download [Leiningen][lein-unix] (save as "lein")

- Make "lein" executable: `chmod +x lein`

- Download retrospect source:

    git clone git@bitbucket.org:joshuaeckroth/retrospect.git

- Go into the `retrospect` directory: `cd retrospect`

- Download the [data package][data] and unzip (creating the `data`
  folder)

- Start the retrospect player (as a test):

    lein run -m retrospect.core --database "http://fier.ath.cx:5984/retrospect"

## Installation (Windows)

- Download [Leiningen][lein-win] (open the zip file, extract
  somewhere)

- Run `lein self-install` in a command prompt (in the newly extracted
  folder)

- Set up git with your SSH keys

- Download retrospect source (in a command prompt):
  `git clone git@bitbucket.org:joshuaeckroth/retrospect.git`

- Go into the `retrospect` directory: `cd retrospect`

- Download the [data package][data] and unzip (creating the `data`
  folder)

- Start the retrospect player (as a test):

    lein run -m retrospect.core --database "http://fier.ath.cx:5984/retrospect"
    
## Eclipse IDE plugin: Counterclockwise

- Download and unzip [Eclipse for Java developers](http://eclipse.org/downloads/)

- Add the Counterclockwise update site to Eclipse's software sources:
  http://ccw.cgrand.net/updatesite
  
- Install the Counterclockwise plugin just like any other Eclipse plugin
  
- Perhaps watch the getting-started videos on the [Counterclockwise
  website](http://code.google.com/p/counterclockwise/)
  
- Create a new Clojure project

- Uncheck "Use default location" and provide the location of the
  existing `retrospect` folder
  
I recommend you modify source files in Eclipse but run the code on the
command line (using the usual `lein run ...` commands).
    
## Running

### Player interface

Load the tracking GUI:

    lein run -m retrospect.core --database "http://fier.ath.cx:5984/retrospect"
    
Load the words GUI:

    lein run -m retrospect.core --problem words --database "http://fier.ath.cx:5984/retrospect"
    
### Batch runs

Required arguments:

- `--action run`

- `--params Tracking/foobar`

- `--database "http://fier.ath.cx:5984/retrospect"`

Other optional arguments:

- `--nthreads 4` number of threads for parallel execution (default is
  1)

- `--datadir data` where the data files are stored (default is `data`)

- `--recordsdir records` where the output is stored (default is
  `records`)

- `--repetitions 20` number of random variations for each parameter
  configuration (default is 10)
  
- `--seed 0` starting random seed (default is 0; any integer works)

- `--monitor false` whether or not to use the "monitor" facility
  (default is false); various monitors can be programmed that detect
  specified situations, stop the batch run if the situation occurs,
  and starts the player so that the user may examine the situation

[lein-unix]: https://raw.github.com/technomancy/leiningen/stable/bin/lein
[lein-win]: https://github.com/downloads/technomancy/leiningen/leiningen-1.5.2-win.zip
[data]: https://bitbucket.org/joshuaeckroth/retrospect/downloads/retrospect-data.zip
