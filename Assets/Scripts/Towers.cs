using UnityEngine;

public class Towers : MonoBehaviour , ITowers
{
    public Transform rotate;
    public Transform firePoint;
    public GameObject bulletPrefab;
    public float range;
    public float rotationSpeed;
    public float fireRate;
    public float contFire;
    public string shoot;
    public string shootAnim;
    public Animator anim;
    private Transform _target;

    private void Start()
    {
        InvokeRepeating("TargetUpdate",0f,0.5f);
    }
    public void TargetUpdate()
    {
        GameObject[] enemies = GameObject.FindGameObjectsWithTag("Enemy");
        GameObject nearestEnemy = null;
        float shortestDistance = Mathf.Infinity;
        
        foreach (GameObject enemy in enemies)
        {
            float distance = Vector3.Distance(transform.position, enemy.transform.position);
            if (distance < shortestDistance)
            {
                shortestDistance = distance;
                nearestEnemy = enemy;
            }
        }
        if (nearestEnemy != null && shortestDistance <= range)
        {
            _target = nearestEnemy.transform;
        }
        else
        {
            _target = null;
        }
    }
    private void Update()
    {
        if (_target == null)
        {
            return;
        }
        
        Rotation();
        
        if (contFire <= 0f)
        {
            Shoot(bulletPrefab);
            contFire = 1f / fireRate;
        }
        contFire -= Time.deltaTime;
    }   
    public void Shoot(GameObject bullet)
    {
        GameObject bulletShoot = (GameObject)Instantiate(bulletPrefab, firePoint.position, firePoint.rotation);
        anim.SetTrigger(shootAnim);
        AudioManager.instance.Play(shoot);
        Bullet _bullet = bulletShoot.GetComponent<Bullet>();
        if (_bullet != null)
        {
            _bullet.Pursuit(_target);
        }
        
    }  
    public void Rotation()
    {
        Vector3 dir = _target.position - transform.position;
        Quaternion lookRotation = Quaternion.LookRotation(dir);
        Vector3 rotation = Quaternion.Lerp(rotate.rotation, lookRotation, Time.deltaTime * rotationSpeed).eulerAngles;
        rotate.rotation = Quaternion.Euler(0f, rotation.y, 0f);
    }
    private void OnDrawGizmosSelected()
    {
        Gizmos.color = Color.cyan;
        Gizmos.DrawWireSphere(transform.position, range);
    }

}
