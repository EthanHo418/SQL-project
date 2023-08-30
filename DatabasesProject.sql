-- PART 1
-- #1 Functional dependency identified: 
-- precinct -> geo, state, locality
-- precinct, Timestamp -> EVERYTHING

-- #2 Penna is NOT in BCNF
CREATE TABLE new_penna
  AS (select distinct * from testDB.penna where geo!= "");


--  removed rows where geo is empty and decompose Penna to BCNF scheme
CREATE TABLE pFK as (Select distinct precinct, geo, locality, state from new_penna);

CREATE TABLE pPK as (select distinct precinct, Timestamp, ID, totalvotes, Biden, Trump, filestamp from new_penna);

-- primary and foreign key

ALTER TABLE pFK
add primary key(precinct);


ALTER TABLE pPK
add foreign key(precinct)
references pFK(precinct)
on delete set null; 




-- PART 2
-- #1a
delimiter //
create procedure Winner(
	in precinct_var text
)
begin
	select
		case
			when Biden > Trump then 'Biden'
			when Trump > Biden then 'Trump'
			else 'Tied'
		end as winner,
		(greatest(Biden, Trump) / totalvotes) * 100 as winning_pct,
		totalvotes
	from
		penna
	where
		precinct = precinct_var
	order by
		Timestamp desc
	limit 1;
end //
delimiter ;

call Winner('LOWER SWATARA TWP--3RD PRECINCT');

-- #1b
delimiter //
create procedure RankALL(
	in precinct_var text
)
begin
		select ranking from (
		select
			precinct,
			rank() over (order by totalvotes desc) as ranking
		from
		(
			select
				*
			from
				(
					select
						*,
						rank() over (partition by precinct order by timestamp desc) as r 
					from
						penna
				) as agg 
			where r = 1
		) as agg_2 ) as agg_3
        where precinct = precinct_var;
end //
delimiter ;
call RankALL('031 W BRADFORD 3');

-- #1c
delimiter //
create procedure RankCounty(
	in precinct_var text
)
begin
		select ranking from (
		select
			precinct,
			rank() over (partition by locality order by totalvotes desc) as ranking
		from
		(
			select
				*
			from
				(
					select
						*,
						rank() over (partition by precinct order by timestamp desc) as r 
					from
						penna
				) as agg 
			where r = 1
		) as agg_2 ) as agg_3
        where precinct = precinct_var;
end //
delimiter ;

call RankCounty('031 W BRADFORD 3');

-- #1d
delimiter //
create procedure PlotPrecinct(
	in precinct_var text
)
begin
	select
		timestamp, 
		totalvotes,
		Biden,
		Trump
    from penna
    where precinct=precinct_var
    order by timestamp;
end //
delimiter ;

call PlotPrecinct('290 HIGHLAND');

-- #1e

delimiter //
create procedure EarliestPrecinct(
	in vote_count int
)
begin
	select
		precinct, timestamp
	from
		penna
	where
		totalvotes > vote_count
	order by
		Timestamp, totalvotes desc
	limit 1;
end //
delimiter ;

call EarliestPrecinct(1137);


-- #2a
delimiter //
create procedure PrecinctsWon(
	in candidate varchar(255)
)
begin
	select
		precinct,
        abs(Biden - Trump) as diff,
        case
			when candidate = 'Biden' then Biden
            when candidate = 'Trump' then Trump
		end as winner_votes
	from
		(
			select
				*,
				rank() over (partition by precinct order by timestamp desc) as r 
			from
				penna
		) as agg 
	where
        case
			when candidate = 'Biden' then Biden > Trump
			when candidate = 'Trump' then Trump > Biden
		end
        AND r = 1
	order by
		diff desc;
end //
delimiter ;


call PrecinctsWon('Biden');



-- #2b
delimiter //
create procedure PrecinctsWonCount(
	in candidate varchar(255)
)
begin
	select
		count(1)
	from
		(
			select
				*,
				rank() over (partition by precinct order by timestamp desc) as r 
			from
				penna
		) as agg 
	where
		r = 1
        and case
			when candidate = 'Biden' then Biden > Trump
            when candidate = 'Trump' then Trump > Biden
		end;
end //
delimiter ;

call PrecinctsWonCount('Biden');
call PrecinctsWonCount('Trump');

-- #2c
delimiter //
create procedure PrecinctsFullLead(
	in candidate varchar(255)
)
begin
	select
		precinct
	from
		penna
	group by
		precinct
	having
		case
			when candidate = 'Biden' then count(1) = sum(Biden > Trump)
            when candidate = 'Trump' then count(1) = sum(Trump > Biden)
		end;
end //
delimiter ;

call PrecinctsFullLead('Biden');
call PrecinctsFullLead('Trump');


-- #2d
delimiter //
create procedure PlotCandidate(
	in candidate varchar(255)
)
begin
	select
		Timestamp,
        case
			when candidate = 'Biden' then sum(Biden)
            when candidate = 'Trump' then sum(Trump)
		end as votes
	from
		penna
	group by
		Timestamp
	order by
		Timestamp;
end //
delimiter ;

call PlotCandidate('Biden');


-- #2e
delimiter //
create procedure PrecinctsWonTownship(
)
begin
	
select
			Winner,
            totalBiden,
            totalTrump,
            diff
from
(select 'Township' as precinctType, 
CASE
        WHEN (ultimatesum.totalTrump > ultimatesum.totalBiden) THEN 'Trump' 
    WHEN (ultimatesum.totalTrump < ultimatesum.totalBiden) THEN 'Biden' 
    ELSE 'TIE'
    
END AS Winner, totalBiden, totalTrump, diff

from (select sum(mt) as totalTrump, sum(mb) as totalBiden,  abs((sum(mt)) - (sum(mb))) as diff
from (select precinct, max(Trump) as mt, max(Biden) as mb
from testDB.penna 
where precinct LIKE '%Township%'
group by precinct) as sumthis) as ultimatesum) as agg;
end //
delimiter ;

call PrecinctsWonTownship();

delimiter //
create procedure PrecinctsWonBorough(
)
begin
	
select
			Winner,
            totalBiden,
            totalTrump,
            diff
from
(select 'Borough' as precinctType, 
CASE
        WHEN (ultimatesum.totalTrump > ultimatesum.totalBiden) THEN 'Trump' 
    WHEN (ultimatesum.totalTrump < ultimatesum.totalBiden) THEN 'Biden' 
    ELSE 'TIE'
    
END AS Winner, totalBiden, totalTrump, diff

from (select sum(mt) as totalTrump, sum(mb) as totalBiden,  abs((sum(mt)) - (sum(mb))) as diff
from (select precinct, max(Trump) as mt, max(Biden) as mb
from testDB.penna 
where precinct LIKE '%Borough%'
group by precinct) as sumthis) as ultimatesum) as agg;
end //
delimiter ;

call PrecinctsWonBorough();

delimiter //



create procedure PrecinctsWonWard(
)
begin
	
select
			Winner,
            totalBiden,
            totalTrump,
            diff
from
(select 'Ward' as precinctType, 
CASE
        WHEN (ultimatesum.totalTrump > ultimatesum.totalBiden) THEN 'Trump' 
    WHEN (ultimatesum.totalTrump < ultimatesum.totalBiden) THEN 'Biden' 
    ELSE 'TIE'
    
END AS Winner, totalBiden, totalTrump, diff

from (select sum(mt) as totalTrump, sum(mb) as totalBiden,  abs((sum(mt)) - (sum(mb))) as diff
from (select precinct, max(Trump) as mt, max(Biden) as mb
from testDB.penna 
where precinct LIKE '%Ward%'
group by precinct) as sumthis) as ultimatesum) as agg;

end //
delimiter ;

call PrecinctsWonWard();

-- #3a

delimiter //
create procedure TotalVotes(
	in timestamp_var text,
    in category_var text
)
begin
	select
		precinct
	from
		penna
	where
		Timestamp = timestamp_var
	order by
		case
			when category_var = 'ALL' then totalvotes
            when category_var = 'Biden' then Biden
            when category_var = 'Trump' then Trump
		end;
end //
delimiter ;

call TotalVotes('2020-11-05 00:16:15', 'ALL');
call TotalVotes('2020-11-05 00:16:15', 'Biden');
call TotalVotes('2020-11-05 00:16:15', 'Trump');


-- #3b
delimiter //
create procedure GainDelta(
	in timestamp_var datetime
)
begin

	select(select sum(totalvotes) from penna where Timestamp = timestamp_var) -
        (select sum(totalvotes) from penna where Timestamp = previous_timestamp) as Gain,
        time_to_sec(timediff(timestamp_var, previous_timestamp)) as Delta_Seconds,
    
		((select sum(totalvotes) from penna where Timestamp = timestamp_var) -
        (select sum(totalvotes) from penna where Timestamp = previous_timestamp))
        / time_to_sec(timediff(timestamp_var, previous_timestamp)) as Ratio
	from
	(
		select
			max(Timestamp) previous_timestamp
		from
			penna
		where
			Timestamp < timestamp_var
	) as agg;

end //
delimiter ;

call GainDelta('2020-11-04 02:21:55');
call GainDelta('2020-11-04 02:33:08');
call GainDelta("2020-11-04 03:58:36");

-- #3c
delimiter //
create function deltaGainRatio (time_var char(50))
returns double deterministic 
begin

return (select
		((select sum(totalvotes) from penna where Timestamp = time_var) -
        (select sum(totalvotes) from penna where Timestamp = previous_timestamp))
        / time_to_sec(timediff(time_var, previous_timestamp)) as Ratio
	from
	(
		select
			max(Timestamp) previous_timestamp
		from
			penna
		where
			Timestamp < time_var
	) as agg);
end //
delimiter ;


delimiter //
create procedure RankTimestamp()
begin
 
        
	select Timestamp, rank() over (order by ratio desc) as ranking
		from (select Timestamp, deltaGainRatio(Timestamp) as ratio
			from (select distinct Timestamp from penna) output) output;
       

end //
delimiter ;

        
call RankTimestamp();

-- #3d


delimiter //
create procedure VotesPerDay(
in day_var int
)
begin
create table if not exists tsMAX 
(
select max(lol.timestamp) as tStamp
from penna lol
group by DATE(lol.timestamp)
);
if day_var < 3 or day_var > 11 
then select "Day input incorrect";
elseif 
day_var = 3 then select 0 as Biden, 0 as Trump, 0 as totalvotes;
else
select votesLS.Biden - votesLS2.Biden as Biden, 
        votesLS.Trump - votesLS2.Trump as Trump,
        votesLS.totalvotes - votesLS2.totalvotes as totalvotes
from 
( 
    select max(vot.timestamp) as tStamp, 
    sum(vot.Biden) as Biden, 
    sum(vot.Trump) as Trump, 
    sum(vot.totalvotes) as totalvotes
    from penna vot, tsMAX mos
    where vot.timestamp = mos.tStamp
    group by vot.timestamp
) votesLS,
    (
    select max(vot.timestamp) as tStamp, 
    sum(vot.Biden) as Biden, 
    sum(vot.Trump) as Trump, 
    sum(vot.totalvotes) as totalvotes
    from penna vot, tsMAX mos
    where vot.timestamp = mos.tStamp
    group by vot.timestamp
    ) votesLS2
where day(votesLS.tStamp) = day_var and day(votesLS2.tStamp) = day_var - 1;
end if;

drop table tsMAX;
end //
delimiter ;

call VotesPerDay(04);

-- SUSPICIOUS OR INTERESTING DATA: 

select sum(totalvotes) from penna where timestamp = "2020-11-09 21:35:31";
select sum(totalvotes) from penna where timestamp ="2020-11-09 22:44:59"; 

-- sum(totalvotes) goes from '2120363' to '2120249' which makes no sense and shouldn't be possible, since votes were lost as time went on.



select totalvotes, Biden, Trump from penna where totalvotes = Biden + Trump;
select * from penna where timestamp = "2020-11-09 22:44:59";
select *, max from penna where timestamp = max(timestamp)<"2020-11-09 22:44:59" order by timestamp desc;

-- Part 3 a TRUE

    
select case when not exists
(select * from penna where totalvotes < Biden + Trump) 
then 'TRUE' else 'FALSE' end as status;

-- Part 3 b TRUE
select case when sum(bad_entry) = 0 then 'TRUE' else 'FALSE' end as status from (
select count(1) as bad_entry from penna where Timestamp < '2020-11-03 00:00:00' OR Timestamp > '2020-11-11 23:59:59') as agg;

-- Part 3 c FALSE
select
	case when sum(bad_entry) = 0 then 'TRUE' else 'FALSE' end as status
from
(
	select
		case
			when a.max_total <= b.min_total AND a.max_biden <= min_biden and a.max_trump <= b.min_trump then 0 
			else 1
		end as bad_entry 
	from 
		(select precinct, max(totalvotes) max_total, max(Biden) max_biden, max(Trump) max_trump from penna where Timestamp < '2020-11-05 00:00:00' group by precinct) as a
	inner join
		(select precinct, min(totalvotes) min_total, min(Biden) min_biden, min(Trump) min_trump from penna where Timestamp > '2020-11-05 00:00:00' group by precinct) as b
		on a.precinct = b.precinct
) as agg;





-- Part 4.1a TABLES BELOW for pFK and pPK


create table updated_tuples_pFK (
	state varchar(2),
    locality varchar(255),
    precinct text,
    geo text
);

create table updated_tuples_pPK (
	Timestamp text,
    ID varchar(255),
    precinct text,
    totalvotes int,
    Biden int,
    Trump int,
    filestamp text
);


create table inserted_tuples_pFK (
	state varchar(2),
    locality varchar(255),
    precinct text,
    geo text
);

create table inserted_tuples_pPK (
	Timestamp text,
    ID varchar(255),
    precinct text,
    totalvotes int,
    Biden int,
    Trump int,
    filestamp text
);


create table deleted_tuples_pFK (
	state varchar(2),
    locality varchar(255),
    precinct text,
    geo text
);

create table deleted_tuples_pPK (
	Timestamp text,
    ID varchar(255),
    precinct text,
    totalvotes int,
    Biden int,
    Trump int,
    filestamp text
);




-- TRIGGERS BELOW


-- INSERT TRIGGERS FOR BOTH TABLES
delimiter //
create trigger insert_pFK before insert on pFK
for each row
begin
	insert into inserted_tuples_pFK
    set
    
	state       =  NEW.state,
	locality    =  NEW.locality,
	precinct    =  NEW.precinct,
	geo         =  NEW.geo;

end; //
delimiter ;


delimiter //
create trigger insert_pPK before insert on pPK
for each row
begin
	insert into inserted_tuples_pPK
    set
    Timestamp   =  NEW.Timestamp,
    ID = NEW.ID,
	precinct    =  NEW.precinct,
	totalvotes  =  NEW.totalvotes,
	Biden       =  NEW.Biden,
	Trump       =  NEW.Trump,
	filestamp   =  NEW.filestamp;
end; //
delimiter ;




-- UPDATE TRIGGERS FOR BOTH TABLES
delimiter //
create trigger update_pFK before update on pFK
for each row
begin
	insert into updated_tuples_pFK
    set
    
	state       =  OLD.state,
	locality    =  OLD.locality,
	precinct    =  OLD.precinct,
	geo         =  OLD.geo;

end; //
delimiter ;

delimiter //
create trigger update_pPK before update on pPK
for each row
begin
	insert into updated_tuples_pPK
    set
    Timestamp   =  OLD.Timestamp,
    ID = NEW.ID,
	precinct    =  OLD.precinct,
	totalvotes  =  OLD.totalvotes,
	Biden       =  OLD.Biden,
	Trump       =  OLD.Trump,
	filestamp   =  OLD.filestamp;
end; //
delimiter ;




-- DELETE TRIGGERS FOR BOTH TABLES

delimiter //
create trigger delete_pFK before delete on pFK
for each row
begin
	insert into deleted_tuples_pFK
    set
	state       =  OLD.state,
	locality    =  OLD.locality,
	precinct    =  OLD.precinct,
	geo         =  OLD.geo;
end; //
delimiter ;


delimiter //
create trigger delete_pPK before delete on pPK
for each row
begin
	insert into deleted_tuples_pPK
    set
    Timestamp   =  OLD.Timestamp,
    ID = OLD.ID,
	precinct    =  OLD.precinct,
	totalvotes  =  OLD.totalvotes,
	Biden       =  OLD.Biden,
	Trump       =  OLD.Trump,
	filestamp   =  OLD.filestamp;
end; //
delimiter ;





-- 4.2a MOVE VOTES
delimiter //
create procedure MoveVotes(
	in precinct_var text,
    in timestamp_var text,
    in from_candidate varchar(255),
    in vote_num int
)
begin
	IF (from_candidate != 'Trump' AND from_candidate !='Biden') THEN
		select 'Wrong Candidate';
	END IF;
    IF (vote_num < 1) THEN
		select 'vote_num should be positive';
	END IF;
    BEGIN
		DECLARE precinct_match_count int;
		select count(1) into precinct_match_count from penna where precinct = precinct_var;
        if (precinct_match_count = 0) THEN
			select 'Unknown precinct';
		end if;
	END;
    BEGIN
		DECLARE timestamp_match_count int;
		select count(1) into timestamp_match_count from penna where Timestamp = timestamp_var;
        if (timestamp_match_count = 0) THEN
			select 'Unknown Timestamp';
		end if;
	END;
    BEGIN
		DECLARE match_count int;
		select count(1) into match_count from penna where precinct = precinct_var and Timestamp = timestamp_var;
        if (match_count != 1) THEN
			select 'None or multiple rows matched precinct & Timestamp condition, invalid data, do nothing';
		end if;
	END;    
    BEGIN
		DECLARE from_candidate_vote int;
		select case when from_candidate = 'Trump' then Trump else Biden end into from_candidate_vote from penna where precinct = precinct_var and Timestamp = timestamp_var;
        if (vote_num > from_candidate_vote) THEN
			select 'Not enough votes';
		end if;
	END;
    if (from_candidate = 'Trump') THEN
		update penna set Trump = Trump - vote_num, Biden = Biden + vote_num where Timestamp >= timestamp_var and precinct = precinct_var;
	else
		update penna set Trump = Trump + vote_num, Biden = Biden - vote_num where Timestamp >= timestamp_var and precinct = precinct_var;
    end if;
end; //
delimiter ;

call MoveVotes('Southmont Borough No. 2 Voting Precinct', '2020-11-11 00:16:54', 'Trump', 223);
-- testing: select * from penna where timestamp ='2020-11-11 00:16:54' and precinct = 'Southmont Borough No. 2 Voting Precinct';


-- INSERT PROCEDURES CHECK
delimiter //
	create procedure INSERTpPK(
	
    in precinct_val text,
    in timestamp_val text,
    in ID_val varchar(255),
    in totalvotes_val int,
    in Biden_val int,
    in Trump_val int,
    in filestamp_val text)

begin

 declare exit handler for sqlexception
 begin
 select 'insertion rejected due to constraint violation' as output;
 end;
 
 
if not ((day(timestamp_val))<='11' and day((timestamp_val))>='03')
then signal sqlstate '23505';
end if;

if not (totalvotes_val>=Biden_val+Trump_val)
then signal sqlstate '23506';
end if;

insert into pPK values(precinct_val, timestamp_val ,ID_val, totalvotes_val ,Biden_val ,Trump_val ,filestamp_val);

end //
delimiter ;




delimiter //
	create procedure INSERTpFK(
		in precinct_val text,
		in geo_val text,
		in locality_val varchar(255),
		in state_val varchar(2)
    )
begin
 declare exit handler for sqlexception
 begin
 select 'insertion rejected due to constraint violation' as output;
 end;
 insert into pFK values(precinct_val, geo_val, locality_val, state_val);
end //
delimiter ;

-- DELETE PROCEDURES CHECK

delimiter //
	create procedure DELETEpPK(
	
    in precinct_val text,
    in timestamp_val text
)
begin
 declare exit handler for sqlexception
 begin
 select 'delete rejected due to constraint violation' as output;
 end;
 
delete from pPK where pPK.precinct=precinct_val and pPK.timestamp=timestamp_val;

end //
delimiter ;

delimiter //
	create procedure DELETEpFK(
    in precinct_val text
   )
begin
 declare exit handler for sqlexception
 begin
 select 'delete rejected due to constraint violation' as output;
 end;
  delete from pFK where pFK.precinct=precinct_val;

end //
delimiter ;


-- UPDATE PROCEDURES CHECK

delimiter //
	create procedure UPDATEpPK(
	in precinct_old text,
    in timestamp_old text,
    in precinct_val text,
    in timestamp_val text,
    in ID_val varchar(255),
    in totalvotes_val int,
    in Biden_val int,
    in Trump_val int,
    in filestamp_val text)

begin

 declare exit handler for sqlexception
 begin
 select 'update rejected due to constraint violation' as output;
 end;
 
 
if not ((day(timestamp_val))<='11' and day((timestamp_val))>='03')
then signal sqlstate '23505';
end if;

if not (totalvotes_val>=Biden_val+Trump_val)
then signal sqlstate '23506';
end if;

update pPK 
set pPK.precinct=precinct_val,
 pPK.timestamp=timestamp_val ,
 pPK.ID=ID_val, 
 pPK.totalvotes=totalvotes_val ,
 pPK.Biden=Biden_val ,
 pPK.Trump=Trump_val ,
 pPK.filestamp=filestamp_val
 where pPK.precinct = precinct_old and pPK.timestamp = timestamp_old;

end //
delimiter ;



delimiter //
	create procedure UPDATEpFK(
		in precinct_old text,
        in precinct_val text,
		in geo_val text,
		in locality_val varchar(255),
		in state_val varchar(2)
    )
begin
 declare exit handler for sqlexception
 begin
 select 'update rejected due to constraint violation' as output;
 end;
update pFK 
set pFK.precinct=precinct_val,
	pFK.geo=geo_val,
    pFK.locality=locality_val,
    pFK.state=state_val
 where pFK.precinct = precinct_old;
end //
delimiter ;


-- video test stuff A demo video where you show updates (insertions/deletions) 
-- which succeed and which, show triggers as well

 -- FOR pFK precinct, geo, state, locality
 select precinct, geo, state, locality from penna;
  select * from pFK where precinct = 'spaghetti township';

 call INSERTpFK('spaghetti township', '42021-ADAMS TWP DUNLO',  'Cambria','PA');
 
 
 call DELETEpFK('spaghetti township');

 call UPDATEpFK('spaghetti township', 'ice cream township', '42021-ADAMS TWP DUNLO',  'Cambria','PA');
 select * from pFK where precinct = 'ice cream township';
 
 -- FOR pPK precinct, timestamp, ID, totalvotes, Biden, Trump, filestamp
select * from pPK where precinct = 'Adams Township - Dunlo Voting Precinct' and timestamp = '2020-11-04 03:58:37';
call INSERTpPK('Adams Township - Dunlo Voting Precinct'
,'2020-11-04 03:58:37', '1',  '100', '50', '50', 'NOVEMBER_04_2020_035836.json');
 
 call INSERTpPK('Adams Township - Dunlo Voting Precinct'
,'2020-11-12 03:58:37', '1',  '100', '50', '50', 'NOVEMBER_04_2020_035836.json');

 select * from pPK where precinct = 'Adams Township - Dunlo Voting Precinct' and timestamp = '2020-11-04 03:58:37';
 call DELETEpPK ('Adams Township - Dunlo Voting Precinct', '2020-11-04 03:58:37');
 
 call UPDATEpPK('Adams Township - Dunlo Voting Precinct', '2020-11-04 03:58:37','PENN', '2020-11-05 03:58:37','2',  '200', '100', '100', 'NOVEMBER_04_2020_035839.json' );
select * from pPK where precinct = 'PENN' and timestamp = '2020-11-05 03:58:37';

select * from deleted_tuples_pPK;
select * from updated_tuples_pPK;
select * from inserted_tuples_pPK;

