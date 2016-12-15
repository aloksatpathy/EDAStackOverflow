CREATE TABLE #votesUp(PostId int, VoteTypeIdCnt bigint)
INSERT INTO #votesUp SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=2 GROUP BY PostId

CREATE TABLE #votesDown(PostId int, VoteTypeIdCnt bigint)
INSERT INTO #votesDown SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=3 GROUP BY PostId

CREATE TABLE #votesSpam(PostId int, VoteTypeIdCnt bigint)
INSERT INTO #votesSpam SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=12 GROUP BY PostId

select top 500
P.PostTypeId as postTypeId,
CASE WHEN P.AcceptedAnswerId IS NOT NULL
 THEN 1 
 ELSE 0 END as IsAcceptedAnswer,
P.Score as postScore,
LEN(P.Body) as post_length,
P.CreationDate,
P.LastEditDate,
P.LastActivityDate,
DATEDIFF(mi, P.CreationDate, P.LastEditDate) as editDurationAfterCreation,
DATEDIFF(mi, P.CreationDate, P.LastActivityDate) as activityDurationAfterCreation,
LEN(P.Title) as title_length,
LEN(P.Tags)-LEN(REPLACE(P.Tags, '<', '')) as num_tags,
P.CommentCount as num_comment,
P.FavoriteCount as num_favorite,
CASE WHEN CHARINDEX('<code>', P.Body)>0 
 THEN 1 
 ELSE 0 END as has_code,
P.ViewCount as post_views,
U.Reputation as owner_reputation,
U.WebsiteURL as owner_WebsiteURL,
U.Location as owner_Location,
U.AboutMe as owner_AboutMe,
CASE WHEN LTRIM(RTRIM(U.WebsiteURL)) = '' OR U.WebsiteURL IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(U.Location)) = '' OR U.Location IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(U.AboutMe)) = '' OR U.AboutMe IS NULL THEN 0 ELSE 1 END as owner_profile_summary,
U.Views as owner_views,
U.UpVotes as owner_upvotes,
U.DownVotes as owner_downvotes,
DATEDIFF(d, U.LastAccessDate, GetDate()) as owner_lastactivity_days,
VU.VoteTypeIdCnt as upVotes,
VD.VoteTypeIdCnt as downVotes,
VS.VoteTypeIdCnt as isSpam

from PostsWithDeleted as P inner join
Users as U on P.OwnerUserId=U.Id left outer join
#votesUp as VU on VU.PostId=P.Id left outer join
#votesDown as VD on VD.PostId=P.Id left outer join
#votesSpam as VS on VS.PostId=P.Id

where (P.PostTypeId=1 OR P.PostTypeId=2)
and VS.VoteTypeIdCnt>0
