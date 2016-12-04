CREATE TABLE #votesUp(PostId int, VoteTypeIdCnt bigint)

INSERT INTO #votesUp SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=2 GROUP BY PostId



CREATE TABLE #votesDown(PostId int, VoteTypeIdCnt bigint)

INSERT INTO #votesDown SELECT PostId, Count(*) as VoteTypeIdCnt FROM Votes WHERE VoteTypeId=3 GROUP BY PostId


select
AP.Id,
AP.Score as a_score, 
AP.Body as a_body,
AP.ViewCount as a_num_views,
LEN(AP.Body) as a_body_length, 
CASE WHEN CHARINDEX('<code>', AP.Body)>0 
  THEN 1 
  ELSE 0 END as a_body_has_code,
AP.CreationDate as a_CreationDate,
AP.LastEditDate as a_LastEditDate,
AP.LastActivityDate as a_LastActivityDate,
DATEDIFF(d, AP.CreationDate, GetDate()) as a_DaysOld,
CASE WHEN AP.LastEditDate IS NULL
  THEN 0
  ELSE CASE WHEN AP.LastEditDate = AP.CreationDate
          THEN 0
          ELSE 1
          END
  END as a_has_edited,
AP.CommentCount as a_num_comment,
AU.Reputation as a_owner_reputation,
AU.WebsiteURL as a_owner_WebsiteURL,
AU.Location as a_owner_Location,
AU.AboutMe as a_owner_AboutMe,
CASE WHEN LTRIM(RTRIM(AU.WebsiteURL)) = '' OR AU.WebsiteURL IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(AU.Location)) = '' OR AU.Location IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(AU.AboutMe)) = '' OR AU.AboutMe IS NULL THEN 0 ELSE 1 END as a_owner_profile_summary,
AU.Views as a_owner_views,
AU.UpVotes as a_owner_upvotes,
AU.DownVotes as a_owner_downvotes,




QP.Score as q_score,
QP.Body as q_body,
QP.ViewCount as q_num_views,
LEN(QP.Body) as q_body_length,
CASE WHEN CHARINDEX('<code>', QP.Body)>0 
  THEN 1 
  ELSE 0 END as q_body_has_code,
QP.CreationDate as q_CreationDate,
QP.LastEditDate as q_LastEditDate,
DATEDIFF(d, QP.CreationDate, GetDate()) as q_DaysOld,
CASE WHEN QP.LastEditDate IS NULL
  THEN 0
  ELSE CASE WHEN QP.LastEditDate = QP.CreationDate
          THEN 0
          ELSE 1
          END
  END as q_has_edited,
LEN(QP.Title) as q_title_length,
QP.Tags as q_tags,
LEN(QP.Tags)-LEN(REPLACE(QP.Tags, '<', '')) as q_num_tags,
QP.AnswerCount as q_num_answers,
QP.CommentCount as q_num_comment,
QP.FavoriteCount as q_num_favorite,
QU.Reputation as q_owner_reputation,
QU.WebsiteURL as q_owner_WebsiteURL,
QU.Location as q_owner_Location,
QU.AboutMe as q_owner_AboutMe,
CASE WHEN LTRIM(RTRIM(QU.WebsiteURL)) = '' OR QU.WebsiteURL IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(QU.Location)) = '' OR QU.Location IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(QU.AboutMe)) = '' OR QU.AboutMe IS NULL THEN 0 ELSE 1 END as q_owner_profile_summary,
QU.Views as q_owner_views,
QU.UpVotes as q_owner_upvotes,
QU.DownVotes as q_owner_downvotes,
QP.AcceptedAnswerId,
AP.Id,
CASE WHEN QP.AcceptedAnswerId=AP.Id THEN 1 ELSE 0 END as accepted_answer_flag,
AVU.VoteTypeIdCnt as a_votes_up,
AVD.VoteTypeIdCnt as a_votes_down,
QVU.VoteTypeIdCnt as q_votes_up,
QVD.VoteTypeIdCnt as q_votes_down




from 
Posts as AP inner join
Users as AU on AP.OwnerUserId=AU.Id left join
Posts as QP on AP.ParentId=QP.Id inner join
Users as QU on QP.OwnerUserId=QU.Id left outer join
#votesUp as AVU on AVU.PostId=AP.Id left outer join
#votesDown as AVD on AVD.PostId=AP.Id left outer join
#votesUp as QVU on QVU.PostId=QP.Id left outer join
#votesDown as QVD on QVD.PostId=QP.Id




where
AP.PostTypeId=2 and
AP.Id>40800000