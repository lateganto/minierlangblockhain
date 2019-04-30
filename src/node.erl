-module(node).
-export([main/0, loop/4, start/0]).
-define(TIMEOUT, 10000).

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
  sleep(10),
  Nonce = make_ref(),
  Node ! {ping, self(), Nonce},
  receive
    {pong, Nonce} ->
      %io:format("~p, ~p, Sono ancora vivo!!!!!~n", [Main, Node]),
      watch(Main,Node)
  after 2000 ->
    Main ! {dead, Node}
  %io:format("~p, ~p, Vedi che sono morto!!!!!!~n", [Main, Node])
  end.

loop(Friends, TToMine, TMined, Chain) ->
  case length(Friends) of
    % se non ho amici, chiedo al teacher node
    0 ->
      global:send(teacher_node, {get_friends, self(), make_ref()});

    % TODO avoid loop if there are less then 3 nodes on the net
    % se ne ho un numero minore compreso tra 1 e 2 chiedo ad uno dei miei amici
    N when N<3 ->
      % sceglo a caso uno tra gli amici e mando una richiesta di amicizia
      %io:format("~p, asking friends for new friends~n", [self()]),
      AskToFriend = lists:nth(rand:uniform(length(Friends)), Friends),
      AskToFriend ! {get_friends, self(), make_ref()};
    _ ->
      ok
  end,

  receive
  % se ricevo un ping rispondo con un pong al mittente
    {ping, Sender, Nonce} ->
      Sender ! {pong, Nonce},
      loop(Friends, TToMine, TMined, Chain);

  % se gli altri nodi mi chiedono amici mando semplicemente la lista di amici che ho
    {get_friends, Sender, Nonce} ->
      Sender ! {friends, Nonce, Friends},
      loop(Friends, TToMine, TMined, Chain);

  % il teacher node o gli altri amici mi rispondono con una lista di amici
    {friends, _, Sent_Friends} ->
      % elimino il PID del mio nodo e degli amici (se presenti)
      New_Friends = Sent_Friends -- [self() | Friends],
      % qui viene salvata la nuova lista di amici
      MyNewFriends = addFriends(Friends, New_Friends),
      %io:format("~p My new friends are: ~p~n", [self(), MyNewFriends]),
      loop(MyNewFriends, TToMine, TMined, Chain);

    {dead, Node} ->
      %uno degli amici e' morto, ne devo acquisire un altro
      io:format("~p, Dead node ~p~n",[self(), Node]),

      %scelgo uno a caso dei nodi rimanenti e chiedo per nupv amici
      %FriendToAsk = lists:nth(rand:uniform(length(Friends) - 1), Friends -- [Node]),
      %FriendToAsk ! {get_friends, self(), make_ref()},

      %TODO non si eliminano i nodi, perche'?
      loop(Friends -- [Node], TToMine, TMined, Chain);


  %%%%%%%%%%%%%%%%%%%%%%%   TRANSACTION   %%%%%%%%%%%%%%%%%%%%%%%%%%

  % ricevo una transazione {ID, Payload}, se l'ID non e' già presente la ritrasmetto agli
  % amici e cerco di inserirla nel prossimo blocco da minare
    {push, T} ->
      NewTransactions =
        case lists:member(T, TToMine) of
          true ->
            %io:format("~p, transazione già presente!~n", [self()]),
            TToMine;
          false ->
            % invio la transazione agli amici e la aggiungo alle transazioni da minare
            spawn(fun() -> sendTransToFriends(T, Friends) end),
            [T | TToMine]
        end,
      loop(Friends, NewTransactions, TMined, Chain);

  %%%%%%%%%%%%%%%%%%%%%%%   BLOCK   %%%%%%%%%%%%%%%%%%%%%%%%%%

    {update, Sender, Block} ->
      % verifico se il blocco non è già nella mia catena
      case lists:member(Block, Chain) of
        true ->
          %io:format("Blocco già presente~n"),
          loop(Friends, TToMine, TMined, Chain);
        false->
          % verifico se il blocco è corretto
          {_,IdPrevBlock,Transactions,Solution} = Block,
          case proof_of_work:check({IdPrevBlock, Transactions}, Solution) of
            true ->
              %blocco corretto, lo ritrasmetto agli amici
              spawn(fun() -> sendBlockToFriends(Block, Friends) end),

              % todo aggiornare visione catena




              loop(Friends, TToMine, TMined, Chain);
            false ->
              % blocco non corretto
              invalid_block,
              loop(Friends, TToMine, TMined, Chain)
          end
      end;

  %%%%%%%%%%%%%%%%%%%%%%%   MINER   %%%%%%%%%%%%%%%%%%%%%%%%%%

  % c'è almeno una transazione da minare e il miner è pronto a prendere transazioni per costruire blocchi
    {miner_ready} when length(TToMine) > 0 ->
      TInMining = lists:sublist(TToMine, 10), % TODO attenzione
      Self = self(),
      case length(Chain) of
        % la catena è vuota, questo blocco sarebbe il primo
        0 ->
          spawn(fun() -> createBlock(TInMining, none, Self) end);

        % la catena non è vuota
        _ ->
          [{IdBlock,_,_,_}|_] = Chain,
          spawn(fun() -> createBlock(TInMining, IdBlock, Self) end)
      end,
      loop(Friends, TToMine, TMined, Chain);

  % ho terminato il mining di un nuovo blocco, lo spammo a tutti gli amici, ripristino il minatore
  % e aggiorno la mia visione della catena
    {block_mined, Block} ->
      spawn(fun() -> sendBlockToFriends(Block, Friends) end), %lo invio agli amici
      self() ! {miner_ready},
      {_,_,TransactionsMined,_} = Block,
      loop(Friends, TToMine--TransactionsMined, TMined--TransactionsMined, [Block]++Chain);


  %%%%%%%%%%%%%%%%%%%%%%%   CHAIN   %%%%%%%%%%%%%%%%%%%%%%%%%%

    {getChain} ->
      io:format("~p, chiedo la catena agli amici!~n", [self()]),
      case length(Friends) of
        0 ->
          io:format("~p, non ho amici!~n", [self()]),
          loop(Friends, TToMine, TMined, Chain);
        _ ->
          io:format("~p, chiedo la catena ad un amico!~n", [self()]),
          Friend = lists:nth(rand:uniform(length(Friends)), Friends),
          {NewTMined, NewChain} = getChain(Friend, Friends -- [Friend]),
          loop(Friends, TToMine, NewTMined, NewChain)
      end;

    {getHead, Sender, Nonce} ->
      case length(Chain) of
        0 ->
          io:format("~p, la mia catena e' vuota, non mando nulla!~n", [self()]),
          Sender ! {head, Nonce, empty};
        _ ->
          io:format("~p, la mia catena non e' vuota, mando all'amico la testa!~n", [self()]),
          [H|_] = Chain,
          Sender ! {head, Nonce, H}
      end,
      loop(Friends, TToMine, TMined, Chain);

    {get_previous, Sender, Nonce, IDPreviousBlock} ->
      Block = searchBlock(IDPreviousBlock, Chain),
      case Block of
        none -> stop;
        _ -> Sender ! {previous, Nonce, Block}
      end,
      loop(Friends, TToMine, TMined, Chain)

  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% se la lista dei nuovi amici e' vuota restituisco la lista di amici vecchia
addFriends(Friends, []) ->
  Friends;

%se la lista di miei amici e' vuota devo aggingerne qualcuno
addFriends([], New_Friends) ->
  % scelgo uno tra gli amici a caso
  ToAddFriend = lists:nth(rand:uniform(length(New_Friends)), New_Friends),
  Self = self(),
  spawn(fun() -> watch(Self, ToAddFriend) end),
  addFriends([ToAddFriend], New_Friends -- [ToAddFriend]);

addFriends(Friends, New_Friends) ->
  case length(Friends) < 3 of
    true ->
      ToAddFriend = lists:nth(rand:uniform(length(New_Friends)), New_Friends),
      Self = self(),
      spawn(fun() -> watch(Self, ToAddFriend) end),
      addFriends([ToAddFriend | Friends], New_Friends -- [ToAddFriend]);
    false ->
      Friends
  end.

%%%%%%%%%%%%%%%%%%%%%%%   TRANSACTION   %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%caso base (chain vuota o blocco non trovato)
sendTransToFriends(_, []) ->
  %io:format("[~p] finito invo trans~n", [self()]),
  ok;

sendTransToFriends(T, [F|Friends]) ->
  %io:format("Invio all'amico [~p]~n", [F]),
  F ! {push, T},
  sendTransToFriends(T, Friends -- [F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%   BLOCK   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ho finito di scorrere la catena oppure la catena e' vuota e non ho trovato nulla
searchBlock(_, []) -> none;

searchBlock(IDToSearch, Chain) ->
  [H|T] = Chain,
  {IDBlock,_,_,_} = H,

  case IDToSearch =:= IDBlock of
    true -> H;
    false -> searchBlock(IDToSearch, T)
  end.

sendBlockToFriends(_,[]) ->
  %io:format("[~p] finito invo blocco amici~n", [self()]),
  ok;

sendBlockToFriends(Block, [F| Friends]) ->
  %io:format("invio blocco amico [~p]~n", [F]),
  F ! {update, self(), Block},
  sendBlockToFriends(Block, Friends).

%%%%%%%%%%%%%%%%%%%%%%%%%%   MINER   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% funzione per il mining di un blocco, al termine invia al sender un messaggio con il nuovo blocco minato
createBlock(Transactions, IdPrevBlock, Sender) ->
  Solution = proof_of_work:solve({IdPrevBlock, Transactions}),
  NewBlock = {make_ref(), IdPrevBlock, Transactions, Solution},
  Sender ! {block_mined, NewBlock}.

%%%%%%%%%%%%%%%%%%%%%%%%%%   CHAIN   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ho finito gli amici a cui chiedere, resituisco void
getChain(Friend, []) ->
  {[],[]};

% chiedo la visione della catena ad uno dei miei amici (lo faccio solo all'inizio!)
getChain(Friend, FriendsToAsk) ->
  io:format("~p, chiedo la catena ad un amico a caso!~n", [self()]),
  Friend ! {getHead, self(), make_ref()},
  receive
  % la chain dell'amico e' vuota, allora chiedo ad un altro amico
    {head, Nonce, empty} ->
      io:format("~p, la catena dell'amico e' vuota!~n", [self()]),
      AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
      getChain(AnotherFriend, FriendsToAsk -- [AnotherFriend]);

  % a partire dalla testa vado a ricreare tutta la catena
    {head, Nonce, Block} ->
      io:format("~p, la catena c'e', continuo a costruirla!~n", [self()]),
      {_, IDPreviousBlock, Transactions, _} = Block,
      getRemainingChain(Friend, FriendsToAsk, IDPreviousBlock, [Transactions], [Block])

  %se non ricevo alcuna risposta dall'amico per un tot di tempo chiedo ad un altro amico
  after ?TIMEOUT ->
    case length(FriendsToAsk) of
      0 -> {[],[]};   % restituisco void
      _ ->
        io:format("~p, l'amico non risponde, chiedo ad un altro amico!~n", [self()]),
        AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
        getChain(AnotherFriend, FriendsToAsk -- [AnotherFriend])
    end
  end.

% ho finito la lista di amici, allora restituisco void
getRemainingChain(_,[],_,_,_) ->
  {[],[]};

% caso in cui sono arrivato all'ultimo blocco della catena e quindi la resituisco insieme
% alle transazioni minate (presenti nei blocchi della catena)
getRemainingChain(_, _, none, TMined, Chain) ->
  io:format("~p, finito di scorrere la catena, la restituisco!~n", [self()]),
  {TMined, Chain};

getRemainingChain(Friend, FriendsToAsk, IDPreviousBlock, TMined, Chain) ->
  io:format("~p, chiedo all'amico il precedente blocco!~n", [self()]),
  Friend ! {get_previous, self(), make_ref(), IDPreviousBlock},
  receive
    {previous, Nonce, Block} ->
      io:format("~p, blocco precedente ricevuto!~n", [self()]),
      {_, IDPreviousBlockTemp, Transactions, _} = Block,
      Friend ! {get_previous, self(), make_ref(), IDPreviousBlockTemp},
      getRemainingChain(Friend, FriendsToAsk, IDPreviousBlockTemp, TMined++Transactions, [Block]++Chain)

  %se non ricevo risposta dopo un certo intervallo di tempo rispondo
  after ?TIMEOUT ->
    case length(FriendsToAsk) of
      0 -> {[],[]};   % restituisco void
      _ ->
        AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
        getRemainingChain(AnotherFriend, FriendsToAsk -- [AnotherFriend], IDPreviousBlock, TMined, Chain)
    end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  io:format("~n[~p] - ASKING FOR FRIENDS TO TEACHER~n", [self()]),
  global:send(teacher_node, {get_friends, self(), make_ref()}),
  sleep(5),

  io:format("~n[~p] - ASK TO FRIENDS FOR CHAIN INITIAL CHAIN~n", [self()]),
  self() ! {getChain},

  io:format("~n[~p] - INVOKE MINER~n", [self()]),
  self() ! {miner_ready},

  loop([],[],[],[]).


main() ->
  T = spawn(teacher_node, main, []),
  sleep(5),
  %Tra = [a1,a2,a3,b1,b2,b3,c1,c2,c3,d,e,f,g,h,i],
  %Cha = [{3,2,[c1,c2,c3],1234}, {2,1,[b1,b2,b3],1234}, {1,none,[a1,a2,a3],1234}],
  N1 = spawn(node, start, []),

  N1 ! {push, {make_ref(), "ciao1"}},
  N1 ! {push, {make_ref(), "ciao2"}},
  N1 ! {push, {make_ref(), "ciao3"}},
  N1 ! {push, {make_ref(), "ciao4"}},

  sleep(10),
  N1 ! {push, {make_ref(), "ciao11"}},
  N1 ! {push, {make_ref(), "ciao21"}},
  N1 ! {push, {make_ref(), "ciao31"}},
  N1 ! {push, {make_ref(), "ciao41"}},

  sleep(30),
  N1 ! {push, {make_ref(), "ciao41"}},
  N1 ! {push, {make_ref(), "ciao51"}},
  N1 ! {push, {make_ref(), "ciao61"}},
  N1 ! {push, {make_ref(), "ciao71"}}.



%N1 = spawn(node, start, []),
%sleep(10),
%N2 = spawn(node, start, []),
%N3 = spawn(node, start, []),
%N4 = spawn(node, start, []).

