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
    <players>
      <li><name/> (<class/>)</li>
    </players>
  </ul>

</apply>
