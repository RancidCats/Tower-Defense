using UnityEngine;
using UnityEngine.UI;

public class Enemys : MonoBehaviour , IEnemy
{
    public float life;
    public float speed;
    public int money;
    public Image lifebar;
    public GameObject effectDead;
    public string deadSound;
    private Transform _target;
    private int _index;
    private float _distance = 0.5f;

    public void Start()
    {
        _target = Waypoints.points[0];
        lifebar.fillAmount = life;
    }
    public void Update()
    {
        Move();
    }
    public void Dead()
    {
        AudioManager.instance.Play(deadSound);
        Instantiate(effectDead, transform.position,transform.rotation);
        WaveSpawner.enemies--;
        Destroy(gameObject);
    }
    public void Move()
    {       
        Vector3 dir = _target.position - transform.position;
        transform.Translate(dir.normalized * speed * Time.deltaTime, Space.World);
        transform.LookAt(_target);
        if (Vector3.Distance(transform.position, _target.position) <= _distance)
        {
            NextWaypoint();
        }
    }
    public void NextWaypoint()
    {
        if (_index >= Waypoints.points.Length - 1)
        {
            Dead();
            Stats.life--;
            return;
        }
        _index++;
        _target = Waypoints.points[_index];
    }
    public void TakeDamage(float damage)
    {
        life -= damage;
        lifebar.fillAmount = life / 25;

        if (life <= 0)
        {
            Stats.money += money;
            Dead();
        }
    }
}
