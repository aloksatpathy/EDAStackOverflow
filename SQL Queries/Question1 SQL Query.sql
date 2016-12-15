select * from
(
select 
distinct 
P.Id PostId,
case 
     when V.VoteTypeId=2 then 'Y'
     else 'N'
end as UpVoted,
case 
     when V.VoteTypeId=3 then 'Y'
     else 'N'
end as DownVoted,
P.PostTypeId PostTypeId,
P.Score PostScore,
len(P.Body) post_length,
U.Reputation owner_reputation, 
CASE WHEN LTRIM(RTRIM(U.WebsiteURL)) = '' OR U.WebsiteURL IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(U.Location)) = '' OR U.Location IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(U.AboutMe)) = '' OR U.AboutMe IS NULL THEN 0 ELSE 1 END 
as owner_profile_summary,
U.Views owner_views, 
U.Upvotes owner_upvotes, 
U.Downvotes owner_downvotes,
U.LastAccessDate owner_lastactivity_days,
DATEDIFF(mi, P.CreationDate, P.LastEditDate) as editDurationAfterCreation,
DATEDIFF(mi, P.CreationDate, P.LastActivityDate) as activityDurationAfterCrea,
P.Title,
LEN(P.Tags)-LEN(REPLACE(P.Tags, '<', '')) as q_num_tags,
P.AnswerCount, 
P.CommentCount,
CASE WHEN CHARINDEX('<code>', P.Body)>0 
  THEN 1 
  ELSE 0 END as has_code,
P.ViewCount post_views,
U.Id UserId,  
R.Id ReviewTaskId, R.ReviewTaskTypeId ReviewTaskTypeId,
RR.RejectionReasonId RejectionReasonId, RRT.Name ReviewTaskResultTypes, 
RRT.Description, RTS.Name ReviewTaskState, RTS.Description ReviewTaskStateDescription, 
PT.Name PostTypeName, PHT.Name PostHistoryTypeName, VT.Name VoteTypeName, 
RRR.Id ReviewRejectionFlag, RRR.Name ReviewRejectionReason, RRR.Description ReviewRejectionReasonDescription
from ReviewTasks R, ReviewTaskStates RTS, ReviewTaskResults RR, 
ReviewRejectionReasons RRR, ReviewTaskResultTypes RRT, 
Posts P, Users U, PostTypes PT, PostFeedback PF, VoteTypes VT, Votes V, PostHistory PH, PostHistoryTypes PHT
where R.Id=RR.ReviewTaskId
and R.ReviewTaskStateId = RTS.Id
and RR.RejectionReasonId = RRR.Id
and RR.ReviewTaskResultTypeId = RRT.Id
and R.PostId=P.Id
and P.OwnerUserId=U.Id
and V.PostId=P.Id
and P.PostTypeId=PT.Id
and PF.PostId=P.Id
and PF.VoteTypeId=VT.Id
and PH.PostHistoryTypeId= PHT.Id
and P.Id=PH.PostId
and upper(RRR.Name) LIKE '%SPAM%'
and RTS.Name='Completed'
and RRT.Name in ('Reject','Delete','Recommend Deletion')
and RR.RejectionReasonId is not null
UNION
select SELECTEDNONREJECTED.*,RRR.Id ReviewRejectionFlag, RRR.Name ReviewRejectionReason, RRR.Description ReviewRejectionReasonDescription from 
(select top 45000 * from 
(select 
distinct 
P.Id PostId,
case
     when V.VoteTypeId=12 then 1
     else 0
end as IsSpam,
case 
     when V.VoteTypeId=2 then 'Y'
     else 'N'
end as UpVoted,
case 
     when V.VoteTypeId=3 then 'Y'
     else 'N'
end as DownVoted,
P.PostTypeId PostTypeId,
P.Score PostScore,
len(P.Body) post_length,
U.Reputation owner_reputation, 
CASE WHEN LTRIM(RTRIM(U.WebsiteURL)) = '' OR U.WebsiteURL IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(U.Location)) = '' OR U.Location IS NULL THEN 0 ELSE 1 END +
CASE WHEN LTRIM(RTRIM(U.AboutMe)) = '' OR U.AboutMe IS NULL THEN 0 ELSE 1 END 
as owner_profile_summary,
U.Views owner_views, 
U.Upvotes owner_upvotes, 
U.Downvotes owner_downvotes,
U.LastAccessDate owner_lastactivity_days,
DATEDIFF(mi, P.CreationDate, P.LastEditDate) as editDurationAfterCreation,
DATEDIFF(mi, P.CreationDate, P.LastActivityDate) as activityDurationAfterCrea,
P.Title,
LEN(P.Tags)-LEN(REPLACE(P.Tags, '<', '')) as q_num_tags,
P.AnswerCount, 
P.CommentCount, 
CASE WHEN CHARINDEX('<code>', P.Body)>0 
  THEN 1 
  ELSE 0 END as has_code,
P.ViewCount post_views,
U.Id UserId,  
R.Id ReviewTaskId, R.ReviewTaskTypeId ReviewTaskTypeId,
RR.RejectionReasonId RejectionReasonId, RRT.Name ReviewTaskResultTypes, 
RRT.Description, RTS.Name ReviewTaskState, RTS.Description ReviewTaskStateDescription, 
PT.Name PostTypeName, PHT.Name PostHistoryTypeName, VT.Name VoteTypeName
from ReviewTasks R, ReviewTaskStates RTS, ReviewTaskResults RR, ReviewTaskResultTypes RRT, 
Posts P, Users U, PostTypes PT, PostFeedback PF, Votes V, VoteTypes VT, PostHistory PH, PostHistoryTypes PHT
where R.Id=RR.ReviewTaskId
and R.ReviewTaskStateId = RTS.Id
--and RR.RejectionReasonId = RRR.Id
and RR.ReviewTaskResultTypeId = RRT.Id
and R.PostId=P.Id
and P.OwnerUserId=U.Id
and V.PostId=P.Id
and P.PostTypeId=PT.Id
and PF.PostId=P.Id
and PF.VoteTypeId=VT.Id
and PH.PostHistoryTypeId= PHT.Id
and P.Id=PH.PostId
and RTS.Name='Completed'
and RRT.Name not in ('Reject','Delete','Recommend Deletion')
and RR.RejectionReasonId is  null
) NOTREJECTED
) SELECTEDNONREJECTED
LEFT OUTER JOIN
ReviewRejectionReasons RRR 
ON SELECTEDNONREJECTED.RejectionReasonId = RRR.Id
)
DATA
order by PostId
--OFFSET 50000 ROWS
