

cd ~/data/LANL
sqlite3 netflow.db

------------------------------------------
-- getting started ONCE ONLY:

CREATE TABLE flows(
       time INT,
       duration INT,
       src VARCHAR(10),
       src_port VARCHAR(6),
       dst VARCHAR(10),
       dst_port VARCHAR(6),
       proto INT,
       packets INT,
       bytes INT
);
.separator ","
.import flows.csv flows
.schema flows

------------------------------------------
-- import the list of selected boxes as used by the 201511 blog:

CREATE TABLE bbox(
       box VARCHAR(10)
);
.import bbox.txt bbox
.schema bbox

CREATE TABLE subflows(
       time INT,
       duration INT,
       src VARCHAR(10),
       src_port VARCHAR(6),
       dst VARCHAR(10),
       dst_port VARCHAR(6),
       proto INT,
       packets INT,
       bytes INT
);

INSERT INTO subflows
	SELECT * FROM flows WHERE src IN bbox AND dst IN bbox;

------------------------------------------
-- initial EDA...

-- how many flows? 
SELECT count(*) FROM subflows;
— 129,977,412 for full flow set
— 129,925,366 for subflow set

-- protocol distribution?
SELECT proto,count(proto) FROM subflows GROUP BY proto;


-- time range:
SELECT (max(time)-min(time))/3600/24 FROM subflows;
-- 36 days

-- how many distinct computers?
SELECT count(*) FROM 
      (SELECT distinct(src) FROM subflows UNION SELECT distinct(dst) FROM subflows);
-- 10109

------------------------------------------
-- nr flows per box…

select src,count(*) from (select src from flows) group by src;

select dst,count(*) from (select dst from flows) group by dst;



------------------------------------------
-- starting to build the graph...


-- how many distinct edges?

CREATE TABLE edges(
       src VARCHAR(10),
       dst VARCHAR(10),
       n_flows INT,
	n_bytes INT,
       port_cnt
);
INSERT INTO edges
       SELECT src,dst,COUNT(*),SUM(bytes),COUNT(DISTINCT(dst_port)) 
       FROM subflows GROUP BY src,dst;

— 157768 edges in total
— now view in R:

.mode csv
.output 'edges.csv'
SELECT * FROM edges;
.output 'stdout'



