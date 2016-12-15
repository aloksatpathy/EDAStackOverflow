CREATE TABLE #votesUp(PostId int, VoteTypeIdCnt bigint)
INSERT INTO #votesUp SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=2 GROUP BY PostId

CREATE TABLE #votesDown(PostId int, VoteTypeIdCnt bigint)
INSERT INTO #votesDown SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=3 GROUP BY PostId

select
C.Id as CommentId, 
C.Score as CommentScore, 
C.Text as CommentText, 
DATEDIFF(d, C.CreationDate, GetDate()) as CommentCrDays,
LEN(C.Text) as CommentLength,
C.CreationDate as CommentCrDt,
C.UserId as CommentUserId,
CU.Reputation as comment_owner_reputation,
CASE WHEN LTRIM(RTRIM(CU.WebsiteURL)) = '' OR CU.WebsiteURL IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(CU.Location)) = '' OR CU.Location IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(CU.AboutMe)) = '' OR CU.AboutMe IS NULL THEN 0 ELSE 1 END as comment_owner_profile_summary,
CU.Views as comment_owner_views,
CU.UpVotes as comment_owner_upvotes,
CU.DownVotes as comment_owner_downvotes,
DATEDIFF(d, CU.LastAccessDate, GetDate()) as comment_owner_lastactivity_days,
P.Id as PostId,
DATEDIFF(d, P.CreationDate, P.LastEditDate) as editDurationAfterCreation,
DATEDIFF(d, P.CreationDate, P.LastActivityDate) as activityDurationAfterCreation,
LEN(P.Title) as title_length,
LEN(P.Tags)-LEN(REPLACE(P.Tags, '<', '')) as num_tags,
P.AnswerCount as PostAnswerCount,
P.FavoriteCount as num_favorite,
CASE WHEN CHARINDEX('<code>', P.Body)>0 
 THEN 1 
 ELSE 0 END as hascode,
P.ViewCount as post_views,
P.PostTypeId as postTypeId,
P.Id,
P.AcceptedAnswerId,
CASE WHEN QP.AcceptedAnswerId=P.Id THEN 1 ELSE 0 END as IsAcceptedAnswer,
P.Score as postScore,
LEN(P.Body) as post_length,
P.CommentCount as PostCommentCount,
P.CreationDate as PostCrDt,
VU.VoteTypeIdCnt as PostUpVotes, 
VD.VoteTypeIdCnt as PostDownVotes
from 
Comments as C inner join  
Posts as P on C.PostId=P.Id inner join
Users as CU on CU.Id=C.UserId inner join
Users as PU on PU.Id=P.OwnerUserId left outer join
Posts as QP on P.ParentId=QP.Id left outer join
#votesUp as VU on VU.PostId=P.Id left outer join
#votesDown as VD on VD.PostId=P.Id
where 
(P.Tags like '%<python>%' or P.Tags like '%<java>%' or P.Tags like '%<javascript>%' or P.Tags like '%<c++>%'
or QP.Tags like '%<python>%' or QP.Tags like '%<java>%' or QP.Tags like '%<javascript>%' or QP.Tags like '%<c++>%')
and P.CreationDate > '2016-01-01 00:00:00' and P.commentCount>0 and
((P.PostTypeId=1 and P.AnswerCount>3) or (P.PostTypeId=2 and QP.AnswerCount>3))
and P.Id>35000000
