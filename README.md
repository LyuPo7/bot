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
                <li><b>error</b>-level(default): show only error messages;</li>
                <li><b>warning</b>-level: show only error and warning messages;</li>
                <li><b>info</b>-level: show info, error and warning messages;</li>
                <li><b>debug</b>-level: show even more details than info-level;</li>
            </ul>
        </li>
    </ul>

<h3>Installation:</h3>
    <p>You may clone GitHub repository</p>
        <p><b>git clone https://github.com/LyuPo7/bot.git</b></p>
 
<h3>How to use?</h3>
    <ol>
        <li> <h4>Setup data/config.json (for example see config-json-file: 'data/config_tele.json' for Telegram and 'data/config_vk.json' for Vk)</h4>
             <ul>
                 <li><b>"api_settings"</b></li>
                    <ul>
                        <li><b>"bot_api":</b> must be one of ["vk", "telegram"];</li>
                        <li><b>"bot_token":</b> token for vk/telegram bot;</li>
                        <li><b>"bot_initial_reply_number":</b> will be used as initial reply number for any new chat;</li>
                        <li><b>"bot_question":</b> quetion in reply to /repeat command;</li>
                        <li><b>"bot_description":</b> message in reply to /help command;</li> 
                    </ul>
             </ul>
            <ul>
                 <li><b>"logger_settings"</b></li>
                    <ul>
                        <li><b>"verbocity":</b> level of logging - must be one of ["debug", "info", "warning", "error"];</li>
                    </ul>
             </ul>
       </li>
       <li><h4>Build project using <b>stack</b>:</h4>
           <ul><b>$ stack build</b>
           </ul>
       </li>
       <li><h4>Run project using <b>stack</b>:</h4>
           <ul><b>$ stack exec bot-exe</b>
           </ul>
               <p>For more information see asciinema below:
                  <a href="https://asciinema.org/a/444443" target="_blank"><img src="https://asciinema.org/a/444443.svg" /></a></b>
               </p>
           </ul>
       </li>
    </ol>
    <p>
    </ol>
<h3>Project pattern</h3>
