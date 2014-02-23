<apply template="base">

  <h3>Game in <stage/></h3>

  <h4>Players</h4>
  <ul>
    <li>
      <form action="/game/${id}/add_player">
        <label for="input_name">Name:</label>
        <input id="input_name" name="name" type="text"/>
        <button type="submit">Add</button>
      </form>
    </li>
    <li><a href="/game/${id}/roll"><button>Roll</button></a> (The first player is up next).</li>
    <players>
      <li><name/> (<class/>) has <assets/> assets.</li>
    </players>
  </ul>

  <h4>Board</h4>
  <board>
    <div class="place">
      <render/>
      <hr/>
      <players>
        <is-place id="${position}">
          <name/>
        </is-place>
      </players>
    </div>
  </board>

</apply>
