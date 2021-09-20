# Echo-bot

[![Haskell CI](https://github.com/LyuPo7/bot/actions/workflows/haskell.yml/badge.svg)](https://github.com/LyuPo7/bot/actions/workflows/haskell.yml)

<h3>Project Description:</h3>
    <p>Echo-bot for messagers</p>
    <ul>
        <li><b>Resends user's message to user;</b></li>
        <li><b>Allows the user to choose number of bot replies;</b></li>
        <li>Supports 2 api:
          <ul>
             <li><b>telegram;</b></li>
             <li><b>vk;</b></li>
            </ul>
        </li>
        <li>Supports 4 levels of logging:
            <ul>
                <li><b>error</b>-level (default): show only error messages;</li>
                <li><b>warning</b>-level: show only error and warning messages;</li>
                <li><b>info</b>-level: show info messages for every step of downloading page including error and warning messages;</li>
                <li><b>debug</b>-level: show even more details than info-level;</li>
            </ul>
        </li>
    </ul>

<h3>Installation:</h3>
    <p>You may clone GitHub repository</p>
        <p><b>git clone https://github.com/LyuPo7/bot</b></p>
 
<h3>How to use?</h3>
    <p>Use <b>page-loader</b> command with appropriate keys.</p>
    <ul>
        <li> <h4>Usage without keys</h4>
            <p>Download html page to work directory</p>
            <a href="https://asciinema.org/a/374204" target="_blank"><img src="https://asciinema.org/a/374204.svg" /></a>
       </li>
        <li> <h4>Usage with output key</h4>
            <p>Download html page to directory in <b>output</b> key</p>
            <a href="https://asciinema.org/a/374207" target="_blank"><img src="https://asciinema.org/a/374207.svg" /></a>
       </li>
        <li> <h4>Usage with <b>verbosity</b> key</h4>
           <ul>
                <li> <p><b>Verbosity = error/warning</b></p>
                    <p>Show error/warning messages only if where was problems while downloading page</p>
                </li>
                <li> <p><b>Verbosity = info</b></p>
                    <p>Show info messages for every step of downloading page</p>
                    <a href="https://asciinema.org/a/374211" target="_blank"><img src="https://asciinema.org/a/374211.svg" /></a>
                </li>
                <li> <p><b>Verbosity = debug</b></p>
                    <p>Show debug messages about every step of downloading page</p>
                    <a href="https://asciinema.org/a/374212" target="_blank"><img src="https://asciinema.org/a/374212.svg" /></a>
                </li>
           </ul>
       </li>
    </ul>
