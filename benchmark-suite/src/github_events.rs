use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
#[serde(transparent)]
pub struct GithubEvents(pub Vec<Event>);

impl crate::Dataset for GithubEvents {
    fn name(&self) -> String {
        "github_events".to_owned()
    }

    fn is_dynamic(&self) -> bool {
        false
    }

    fn load() -> Self {
        let events: Vec<EventJson> = serde_json::from_str(JSON).unwrap();
        Self(events.into_iter().map(Event::from).collect())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EventId(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActorId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RepoId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct OrgId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RepositoryId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PushId(pub u64);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Actor {
    pub id: ActorId,
    pub login: String,
    pub display_login: String,
    pub gravatar_id: String,
    pub url: String,
    pub avatar_url: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Repo {
    pub id: RepoId,
    pub name: String,
    pub url: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Organization {
    pub id: OrgId,
    pub login: String,
    pub gravatar_id: String,
    pub url: String,
    pub avatar_url: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommonEventFields {
    pub id: EventId,
    pub actor: Actor,
    pub repo: Repo,
    #[serde(rename = "public")]
    pub public_: bool,
    pub created_at: DateTime<Utc>,
    #[serde(default)]
    pub org: Option<Organization>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum EventJson {
    PushEvent {
        #[serde(flatten)]
        common: CommonEventFields,
        payload: PushPayload,
    },
    DeleteEvent {
        #[serde(flatten)]
        common: CommonEventFields,
        payload: DeletePayload,
    },
    CreateEvent {
        #[serde(flatten)]
        common: CommonEventFields,
        payload: CreatePayload,
    },
}

impl From<EventJson> for Event {
    fn from(value: EventJson) -> Self {
        match value {
            EventJson::PushEvent { common, payload } => Event::PushEvent { common, payload },
            EventJson::DeleteEvent { common, payload } => Event::DeleteEvent { common, payload },
            EventJson::CreateEvent { common, payload } => Event::CreateEvent { common, payload },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Event {
    PushEvent {
        common: CommonEventFields,
        payload: PushPayload,
    },
    DeleteEvent {
        common: CommonEventFields,
        payload: DeletePayload,
    },
    CreateEvent {
        common: CommonEventFields,
        payload: CreatePayload,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PushPayload {
    pub repository_id: RepositoryId,
    pub push_id: PushId,
    #[serde(rename = "ref")]
    pub ref_: String,
    pub head: String,
    pub before: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RefType {
    Branch,
    Tag,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PusherType {
    User,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeletePayload {
    #[serde(rename = "ref")]
    pub ref_: String,
    pub ref_type: RefType,
    pub full_ref: String,
    pub pusher_type: PusherType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreatePayload {
    #[serde(rename = "ref")]
    pub ref_: String,
    pub ref_type: RefType,
    pub full_ref: String,
    pub master_branch: String,
    pub description: Option<String>,
    pub pusher_type: PusherType,
}

const JSON: &str = include_str!("../data/github_events.json");
